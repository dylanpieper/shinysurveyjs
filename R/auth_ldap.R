#' LDAP Authentication Class
#' @export
LdapAuth <- R6::R6Class(
  "LdapAuth",
  public = list(
    #' @field config LDAP configuration
    config = NULL,
    #' @field authenticated Authentication status
    authenticated = FALSE,
    #' @field user_info User information
    user_info = NULL,

    #' Initialize LDAP Authentication
    #' @param host LDAP server hostname
    #' @param base_dn Base Distinguished Name
    #' @param port LDAP port (default: 389)
    #' @param user_attr User attribute (default: "uid")
    #' @param domain Domain for UPN binding (e.g., "pitt.edu")
    #' @param ssh_tunnel Local port number for SSH tunnel (assumes tunnel already running, e.g., `ssh_tunnel = 3389`)
    #' @param db_ops Database operations object for session management
    #' @param session_duration_days Number of days sessions remain valid (default: 7)
    #' @param auth_table Name of authentication sessions table (default: "sjs_auth")
    initialize = function(host, base_dn, port = 389, user_attr = "uid", domain = NULL, ssh_tunnel = NULL, db_ops = NULL, session_duration_days = 7, auth_table = "sjs_auth") {
      self$config <- list(
        host = host,
        base_dn = base_dn,
        port = port,
        user_attr = user_attr,
        domain = domain,
        ssh_tunnel = ssh_tunnel,
        db_ops = db_ops,
        session_duration_days = session_duration_days,
        auth_table = auth_table
      )

      # Set up SSH tunnel if configured (simple local case only)
      if (!is.null(ssh_tunnel) && is.numeric(ssh_tunnel)) {
        self$config$ssh_tunnel <- list(enabled = TRUE, local_port = ssh_tunnel)
        message("SSH tunnel configured for localhost:", ssh_tunnel)
      }

      # Initialize session table once during app startup if db_ops is provided
      # This ensures the table exists for all future operations without repeated checks
      if (!is.null(db_ops)) {
        self$ensure_session_table_once()
      }
    },

    #' Ensure session table exists once during initialization (no read check)
    #' @description Creates the authentication session table if it doesn't exist and performs initial cleanup
    ensure_session_table_once = function() {
      if (is.null(self$config$db_ops)) {
        return(FALSE)
      }

      tryCatch(
        {
          # Create table if it doesn't exist (using CREATE TABLE IF NOT EXISTS)
          create_query <- paste0("
        CREATE TABLE IF NOT EXISTS ", self$config$auth_table, " (
          token VARCHAR(255) PRIMARY KEY,
          username VARCHAR(255) NOT NULL,
          created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          expires_at TIMESTAMP NOT NULL
        )")

          self$config$db_ops$operate(function(conn) {
            DBI::dbExecute(conn, create_query)
          }, "Failed to create auth_sessions table")

          # Also do initial cleanup of any expired sessions
          self$cleanup_expired_sessions_silent()
          TRUE
        },
        error = function(e) {
          warning("Failed to initialize auth_sessions table: ", e$message)
          FALSE
        }
      )
    },


    #' Generate secure session token
    #' @description Generates a 32-byte random hex token for session authentication
    generate_token = function() {
      # Generate 32 random bytes and convert to hex
      paste0(sprintf("%02x", as.integer(runif(32, 0, 256))), collapse = "")
    },

    #' Create new session
    #' @param username Username to create session for
    create_session = function(username) {
      if (is.null(self$config$db_ops)) {
        return(NULL)
      }

      # Table existence already ensured by validate_session or authentication flow
      token <- self$generate_token()
      expires_at <- Sys.time() + (self$config$session_duration_days * 24 * 60 * 60)

      tryCatch(
        {
          session_data <- data.frame(
            token = token,
            username = username,
            expires_at = expires_at,
            stringsAsFactors = FALSE
          )

          # Insert session data directly using operate method
          self$config$db_ops$operate(function(conn) {
            insert_query <- sprintf(
              "INSERT INTO %s (token, username, expires_at) VALUES (?, ?, ?)",
              self$config$auth_table
            )
            DBI::dbExecute(conn, insert_query, list(token, username, expires_at))
          }, "Failed to insert session")
          token
        },
        error = function(e) {
          warning("Failed to create session: ", e$message)
          NULL
        }
      )
    },

    #' Validate session token
    #' @param token Session token to validate
    validate_session = function(token) {
      if (is.null(self$config$db_ops) || is.null(token) || token == "") {
        return(list(valid = FALSE, user_info = NULL))
      }

      tryCatch(
        {
          # Only clean up expired sessions occasionally (every 12 hours)
          if (is.null(private$last_cleanup) ||
            as.numeric(Sys.time() - private$last_cleanup) > 43200) {
            self$cleanup_expired_sessions()
            private$last_cleanup <- Sys.time()
          }

          # Check if token exists and is not expired
          session_data <- self$config$db_ops$read_table(
            self$config$auth_table,
            filters = list(token = token),
            limit = 1,
            update_last_sql = FALSE
          )

          if (nrow(session_data) > 0) {
            session_row <- session_data[1, ]
            expires_at <- as.POSIXct(session_row$expires_at)

            if (expires_at > Sys.time()) {
              # Session is valid
              self$authenticated <- TRUE
              self$user_info <- list(
                username = session_row$username,
                login_time = as.POSIXct(session_row$created_at),
                token = token
              )

              return(list(valid = TRUE, user_info = self$user_info))
            }
          }

          return(list(valid = FALSE, user_info = NULL))
        },
        error = function(e) {
          warning("Session validation error: ", e$message)
          return(list(valid = FALSE, user_info = NULL))
        }
      )
    },

    #' Clean up expired sessions silently (no logging)
    #' @description Removes expired sessions from the database without warning messages
    cleanup_expired_sessions_silent = function() {
      if (is.null(self$config$db_ops)) {
        return(FALSE)
      }

      tryCatch(
        {
          delete_query <- paste0("DELETE FROM ", self$config$auth_table, " WHERE expires_at < NOW()")
          self$config$db_ops$operate(function(conn) {
            DBI::dbExecute(conn, delete_query)
          }, "Failed to cleanup expired sessions")
          TRUE
        },
        error = function(e) {
          # Silent failure during initialization
          FALSE
        }
      )
    },

    #' Clean up expired sessions
    #' @description Removes expired sessions from the database with error logging
    cleanup_expired_sessions = function() {
      if (is.null(self$config$db_ops)) {
        return(FALSE)
      }

      tryCatch(
        {
          delete_query <- paste0("DELETE FROM ", self$config$auth_table, " WHERE expires_at < NOW()")
          self$config$db_ops$operate(function(conn) {
            DBI::dbExecute(conn, delete_query)
          }, "Failed to cleanup expired sessions")
          TRUE
        },
        error = function(e) {
          warning("Failed to cleanup expired sessions: ", e$message)
          FALSE
        }
      )
    },

    #' Remove specific session
    #' @param token Session token to remove
    logout_session = function(token = NULL) {
      if (is.null(self$config$db_ops)) {
        self$authenticated <- FALSE
        self$user_info <- NULL
        return(TRUE)
      }

      # Use current session token if none provided
      if (is.null(token) && !is.null(self$user_info$token)) {
        token <- self$user_info$token
      }

      if (!is.null(token)) {
        tryCatch(
          {
            delete_query <- paste0("DELETE FROM ", self$config$auth_table, " WHERE token = '", token, "'")
            self$config$db_ops$operate(function(conn) {
              DBI::dbExecute(conn, delete_query)
            }, "Failed to remove session")
          },
          error = function(e) {
            warning("Failed to remove session: ", e$message)
          }
        )
      }

      self$authenticated <- FALSE
      self$user_info <- NULL
      TRUE
    },

    #' Get effective host and port (considering SSH tunnel)
    #' @description Returns connection parameters, using SSH tunnel settings if configured
    get_connection_params = function() {
      if (!is.null(self$config$ssh_tunnel) && isTRUE(self$config$ssh_tunnel$enabled)) {
        list(
          host = "localhost",
          port = self$config$ssh_tunnel$local_port
        )
      } else {
        list(
          host = self$config$host,
          port = self$config$port
        )
      }
    },

    #' Authenticate user credentials
    #' @param username Username
    #' @param password Password
    #' @param logger Optional logger
    authenticate = function(username, password, logger = NULL) {
      if (is.null(username) || is.null(password) || username == "" || password == "") {
        return(list(success = FALSE, message = "Username and password required"))
      }

      tryCatch(
        {
          # Add domain if not present and domain is configured
          if (!is.null(self$config$domain) && !grepl("@", username)) {
            bind_username <- paste0(username, "@", self$config$domain)
            clean_username <- username
          } else {
            bind_username <- username
            clean_username <- gsub("@.*$", "", username)
          }


          # Get connection parameters (may use SSH tunnel)
          conn_params <- self$get_connection_params()

          # Use RCurl to perform LDAP authentication
          ldap_url <- sprintf(
            "ldap://%s:%d/%s??sub?(sAMAccountName=%s)",
            conn_params$host, conn_params$port, self$config$base_dn, clean_username
          )

          # Set up authentication
          auth_string <- paste0(bind_username, ":", password)

          # Use ldapsearch command instead of RCurl (more reliable for AD)
          ldapsearch_cmd <- sprintf(
            'ldapsearch -x -H ldap://%s:%d -D "%s" -w "%s" -b "%s" "(sAMAccountName=%s)" dn 2>/dev/null | grep -q "^dn:"',
            conn_params$host, conn_params$port, bind_username, password, self$config$base_dn, clean_username
          )

          result <- system(ldapsearch_cmd, ignore.stdout = TRUE, ignore.stderr = TRUE) == 0

          if (result) {
            self$authenticated <- TRUE

            # Create session token if database operations are available
            token <- self$create_session(clean_username)

            self$user_info <- list(
              username = username,
              login_time = Sys.time(),
              token = token
            )

            if (!is.null(logger)) {
              logger$log_message(paste("Login success:", username), type = "INFO", zone = "AUTH")
            }

            return(list(success = TRUE, user_info = self$user_info, token = token))
          } else {
            if (!is.null(logger)) {
              logger$log_message(paste("Login failed:", username), type = "WARN", zone = "AUTH")
            }
            return(list(success = FALSE, message = "Invalid credentials"))
          }
        },
        error = function(e) {
          if (!is.null(logger)) {
            logger$log_message(paste("Auth error:", e$message), "ERROR", "AUTH")
          }
          return(list(success = FALSE, message = paste("Authentication error:", e$message)))
        }
      )
    },


    #' Logout user
    #' @description Logs out the current user by removing their session
    logout = function() {
      self$logout_session()
    }
  ),
  private = list(
    last_cleanup = NULL
  )
)

#' Create LDAP login UI
#' @param id Namespace ID
#' @param title Login form title
#' @param logo Logo URL to display instead of title (optional)
#' @param theme_color Hex color code for button styling (optional)
#' @export
ldap_login_ui <- function(id, title = "Login", logo = NULL, theme_color = "#007bff") {
  ns <- shiny::NS(id)

  shiny::div(
    id = ns("login_form"),
    style = "max-width: 400px; margin: 50px auto; padding: 30px; border: 1px solid #ddd; border-radius: 8px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); background: white;",

    # Logo or title header
    if (!is.null(logo)) {
      shiny::div(
        style = "text-align: center; margin-bottom: 30px;",
        shiny::tags$img(src = logo, style = "max-height: 100px; max-width: 300px;")
      )
    } else {
      shiny::h3(title, style = "text-align: center; margin-bottom: 30px; color: #333; font-weight: 500;")
    },

    # Error message container with better styling
    shiny::div(
      id = ns("error_msg"),
      class = "alert alert-danger",
      style = "display: none; margin-bottom: 20px; padding: 12px 16px; border: 1px solid #f5c6cb; border-radius: 4px; background-color: #f8d7da; color: #721c24;",
      shiny::tags$i(class = "fas fa-exclamation-triangle", style = "margin-right: 8px;"),
      shiny::span(id = ns("error_text"))
    ),

    # Username input
    shiny::div(
      style = "margin-bottom: 20px;",
      shiny::tags$label("Username", `for` = ns("username"), style = "display: block; margin-bottom: 5px; font-weight: 500; color: #555;"),
      shiny::tags$input(
        id = ns("username"),
        type = "text",
        placeholder = "Enter your username",
        class = "form-control",
        style = "width: 100%; padding: 12px 16px; border: 1px solid #ddd; border-radius: 4px; font-size: 14px; transition: border-color 0.15s ease-in-out;"
      )
    ),

    # Password input
    shiny::div(
      style = "margin-bottom: 25px;",
      shiny::tags$label("Password", `for` = ns("password"), style = "display: block; margin-bottom: 5px; font-weight: 500; color: #555;"),
      shiny::tags$input(
        id = ns("password"),
        type = "password",
        placeholder = "Enter your password",
        class = "form-control",
        style = "width: 100%; padding: 12px 16px; border: 1px solid #ddd; border-radius: 4px; font-size: 14px; transition: border-color 0.15s ease-in-out;"
      )
    ),

    # Login button with loading state
    shiny::actionButton(
      ns("login_btn"),
      shiny::HTML(paste0('<span id="', ns("login_btn_text"), '">Sign In</span>')),
      class = "btn btn-primary",
      style = paste0("width: 100%; padding: 12px; font-size: 16px; font-weight: 500; background-color: ", theme_color, "; border-color: ", theme_color, "; border-radius: 4px; transition: all 0.15s ease-in-out;")
    ),

    # JavaScript for better UX and cookie management
    shiny::tags$script(shiny::HTML(sprintf('
      // Cookie management functions (global scope)
      function setCookie(name, value, days) {
        var expires = "";
        if (days) {
          var date = new Date();
          date.setTime(date.getTime() + (days * 24 * 60 * 60 * 1000));
          expires = "; expires=" + date.toUTCString();
        }
        document.cookie = name + "=" + (value || "") + expires + "; path=/; SameSite=Strict";
      }

      function getCookie(name) {
        var nameEQ = name + "=";
        var ca = document.cookie.split(";");
        for (var i = 0; i < ca.length; i++) {
          var c = ca[i];
          while (c.charAt(0) == " ") c = c.substring(1, c.length);
          if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length, c.length);
        }
        return null;
      }

      function deleteCookie(name) {
        document.cookie = name + "=; Path=/; Expires=Thu, 01 Jan 1970 00:00:01 GMT;";
      }

      $(document).ready(function() {
        var ns_prefix = "%s";

        // Store session token but do not validate automatically
        $(document).on("shiny:connected", function() {
          var existingToken = getCookie("auth_session");
          if (existingToken) {
            // Store the token but do not trigger validation yet
            window.authSessionToken = existingToken;
          }
        });

        // Handle Enter key on form inputs
        $("#" + ns_prefix + "username, #" + ns_prefix + "password").keypress(function(e) {
          if (e.which === 13) {
            $("#" + ns_prefix + "login_btn").click();
          }
        });

        // Focus management
        $("#" + ns_prefix + "username").focus();

        // Input focus styling
        $("#" + ns_prefix + "username, #" + ns_prefix + "password").focus(function() {
          $(this).css("border-color", "%s");
          $(this).css("box-shadow", "0 0 0 0.2rem %s");
        }).blur(function() {
          $(this).css("border-color", "#ddd");
          $(this).css("box-shadow", "none");
        });

        // Login button click handler
        $(document).on("click", "#" + ns_prefix + "login_btn", function() {
          var btn = $(this);
          var btnText = btn.find("span");

          // Hide any existing error
          $("#" + ns_prefix + "error_msg").hide();

          // Update button to loading state
          btnText.html(\'<i class="fas fa-spinner fa-spin"></i> Signing In...\');
          btn.prop("disabled", true);
          btn.css("opacity", "0.8");
        });
      });

      // Custom message handlers (global scope)
      Shiny.addCustomMessageHandler("show_login_error", function(data) {
        var errorDiv = $("#" + data.id);
        var errorText = $("#" + data.id.replace("error_msg", "error_text"));

        errorText.text(data.message);
        errorDiv.show();

        // Reset login button
        var btn = $("#" + data.id.replace("error_msg", "login_btn"));
        var btnText = btn.find("span");
        btnText.html("Sign In");
        btn.prop("disabled", false);
        btn.css("opacity", "1");
      });

      Shiny.addCustomMessageHandler("login_success", function(data) {
        if (data.token) {
          setCookie("auth_session", data.token, 7);
        }
      });

      Shiny.addCustomMessageHandler("logout_user", function(data) {
        deleteCookie("auth_session");
        location.reload();
      });
    ', ns(""), theme_color, paste0("rgba(", paste(as.integer(col2rgb(theme_color)), collapse = ", "), ", 0.25)"))))
  )
}

#' LDAP login server module
#' @param id Module ID
#' @param ldap_auth LdapAuth instance
#' @param logger Optional logger
#' @export
ldap_login_server <- function(id, ldap_auth, logger = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    # Capture namespace prefix for use in validation function
    ns_prefix <- session$ns("")

    auth_status <- shiny::reactiveValues(
      authenticated = FALSE,
      user_info = NULL
    )

    # Check for existing session token - only validate when authentication is actually needed
    shiny::observeEvent(input$token, {
      # Only validate session if we're not just browsing the library
      # This prevents unnecessary auth checks on the landing page
      if (!is.null(input$token) && input$token != "" && input$validate_session) {
        validation_result <- ldap_auth$validate_session(input$token)

        if (validation_result$valid) {
          auth_status$authenticated <- TRUE
          auth_status$user_info <- validation_result$user_info
          shinyjs::hide("login_form")

          if (!is.null(logger)) {
            username <- validation_result$user_info$username %||% "unknown"
            logger$log_message(paste("Session validated:", username), type = "INFO", zone = "AUTH")
          }

          # Signal successful validation - the auth reactive will handle the rest
        }
      }
    })

    # Method to trigger session validation when needed
    validate_existing_session <- function() {
      shinyjs::runjs(sprintf("
        var existingToken = getCookie('auth_session');
        if (existingToken) {
          Shiny.setInputValue('%svalidate_session', true, {priority: 'event'});
          Shiny.setInputValue('%stoken', existingToken, {priority: 'event'});
        }
      ", ns_prefix, ns_prefix))
    }

    shiny::observeEvent(input$login_btn, {
      shinyjs::hide("error_msg")

      result <- ldap_auth$authenticate(input$username, input$password, logger)

      if (result$success) {
        auth_status$authenticated <- TRUE
        auth_status$user_info <- result$user_info
        shinyjs::hide("login_form")

        # Send session token to client for cookie storage
        if (!is.null(result$token)) {
          session$sendCustomMessage("login_success", list(
            token = result$token
          ))
        }
      } else {
        session$sendCustomMessage("show_login_error", list(
          id = session$ns("error_msg"),
          message = result$message
        ))
      }
    })

    # Return both the auth status reactive and the validation function
    auth_reactive <- shiny::reactive({
      list(
        authenticated = auth_status$authenticated,
        user_info = auth_status$user_info
      )
    })

    # Return a list with both the reactive and the validation function
    return(list(
      auth_status = auth_reactive,
      validate_existing_session = validate_existing_session
    ))
  })
}
