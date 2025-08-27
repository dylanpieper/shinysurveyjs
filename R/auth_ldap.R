#' LDAP Authentication for shinysurveyjs
#'
#' Simple LDAP authentication module that integrates with the survey() function
#'
#' @importFrom R6 R6Class
#' @importFrom shiny div tags observeEvent reactive
#' @importFrom shinyjs show hide
#' @importFrom ldapr ldap

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
    #' @param auth_table Name of authentication sessions table (default: "survey_auth")
    initialize = function(host, base_dn, port = 389, user_attr = "uid", domain = NULL, ssh_tunnel = NULL, db_ops = NULL, session_duration_days = 7, auth_table = "survey_auth") {
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

      # Initialize session table if db_ops is provided
      if (!is.null(db_ops)) {
        self$ensure_session_table()
        # Clean up any expired sessions on startup
        self$cleanup_expired_sessions()
      }
    },

    #' Ensure session table exists
    ensure_session_table = function() {
      if (is.null(self$config$db_ops)) return(FALSE)
      
      tryCatch({
        # Check if table exists by attempting to read from it
        self$config$db_ops$read_table(self$config$auth_table, limit = 0, update_last_sql = FALSE)
      }, error = function(e) {
        # Table doesn't exist, create it
        create_query <- paste0("
        CREATE TABLE IF NOT EXISTS ", self$config$auth_table, " (
          session_token VARCHAR(255) PRIMARY KEY,
          username VARCHAR(255) NOT NULL,
          created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          expires_at TIMESTAMP NOT NULL
        )")
        
        tryCatch({
          self$config$db_ops$operate(function(conn) {
            DBI::dbExecute(conn, create_query)
          }, "Failed to create auth_sessions table")
          TRUE
        }, error = function(e2) {
          warning("Failed to create auth_sessions table: ", e2$message)
          FALSE
        })
      })
    },

    #' Generate secure session token
    generate_session_token = function() {
      # Generate 32 random bytes and convert to hex
      paste0(sprintf("%02x", as.integer(runif(32, 0, 256))), collapse = "")
    },

    #' Create new session
    #' @param username Username to create session for
    create_session = function(username) {
      if (is.null(self$config$db_ops)) return(NULL)
      
      token <- self$generate_session_token()
      expires_at <- Sys.time() + (self$config$session_duration_days * 24 * 60 * 60)
      
      tryCatch({
        session_data <- data.frame(
          session_token = token,
          username = username,
          expires_at = expires_at,
          stringsAsFactors = FALSE
        )
        
        # Insert session data directly using operate method
        self$config$db_ops$operate(function(conn) {
          insert_query <- sprintf(
            "INSERT INTO %s (session_token, username, expires_at) VALUES (?, ?, ?)",
            self$config$auth_table
          )
          DBI::dbExecute(conn, insert_query, list(token, username, expires_at))
        }, "Failed to insert session")
        token
      }, error = function(e) {
        warning("Failed to create session: ", e$message)
        NULL
      })
    },

    #' Validate session token
    #' @param token Session token to validate
    validate_session = function(token) {
      if (is.null(self$config$db_ops) || is.null(token) || token == "") {
        return(list(valid = FALSE, user_info = NULL))
      }
      
      tryCatch({
        # Clean up expired sessions first
        self$cleanup_expired_sessions()
        
        # Check if token exists and is not expired
        session_data <- self$config$db_ops$read_table(
          self$config$auth_table,
          filters = list(session_token = token),
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
              session_token = token
            )
            
            return(list(valid = TRUE, user_info = self$user_info))
          }
        }
        
        return(list(valid = FALSE, user_info = NULL))
      }, error = function(e) {
        warning("Session validation error: ", e$message)
        return(list(valid = FALSE, user_info = NULL))
      })
    },

    #' Clean up expired sessions
    cleanup_expired_sessions = function() {
      if (is.null(self$config$db_ops)) return(FALSE)
      
      tryCatch({
        delete_query <- paste0("DELETE FROM ", self$config$auth_table, " WHERE expires_at < NOW()")
        self$config$db_ops$operate(function(conn) {
          DBI::dbExecute(conn, delete_query)
        }, "Failed to cleanup expired sessions")
        TRUE
      }, error = function(e) {
        warning("Failed to cleanup expired sessions: ", e$message)
        FALSE
      })
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
      if (is.null(token) && !is.null(self$user_info$session_token)) {
        token <- self$user_info$session_token
      }
      
      if (!is.null(token)) {
        tryCatch({
          delete_query <- paste0("DELETE FROM ", self$config$auth_table, " WHERE session_token = '", token, "'")
          self$config$db_ops$operate(function(conn) {
            DBI::dbExecute(conn, delete_query)
          }, "Failed to remove session")
        }, error = function(e) {
          warning("Failed to remove session: ", e$message)
        })
      }
      
      self$authenticated <- FALSE
      self$user_info <- NULL
      TRUE
    },

    #' Get effective host and port (considering SSH tunnel)
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
            session_token <- self$create_session(clean_username)
            
            self$user_info <- list(
              username = username,
              login_time = Sys.time(),
              session_token = session_token
            )

            if (!is.null(logger)) {
              logger$log_message(paste("Login success:", username), type = "INFO", zone = "AUTH")
            }

            return(list(success = TRUE, user_info = self$user_info, session_token = session_token))
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
    logout = function() {
      self$logout_session()
    }
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

        // Check for existing session after Shiny is connected
        $(document).on("shiny:connected", function() {
          var existingToken = getCookie("auth_session");
          if (existingToken) {
            Shiny.setInputValue(ns_prefix + "session_token", existingToken, {priority: "event"});
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
        if (data.session_token) {
          setCookie("auth_session", data.session_token, 7);
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
    auth_status <- shiny::reactiveValues(
      authenticated = FALSE,
      user_info = NULL
    )

    # Check for existing session token
    shiny::observeEvent(input$session_token, {
      if (!is.null(input$session_token) && input$session_token != "") {
        validation_result <- ldap_auth$validate_session(input$session_token)
        
        if (validation_result$valid) {
          auth_status$authenticated <- TRUE
          auth_status$user_info <- validation_result$user_info
          shinyjs::hide("login_form")
          
          if (!is.null(logger)) {
            username <- validation_result$user_info$username %||% "unknown"
            logger$log_message(paste("Session validated:", username), type = "INFO", zone = "AUTH")
          }
        }
      }
    })

    shiny::observeEvent(input$login_btn, {
      shinyjs::hide("error_msg")

      result <- ldap_auth$authenticate(input$username, input$password, logger)

      if (result$success) {
        auth_status$authenticated <- TRUE
        auth_status$user_info <- result$user_info
        shinyjs::hide("login_form")
        
        # Send session token to client for cookie storage
        if (!is.null(result$session_token)) {
          session$sendCustomMessage("login_success", list(
            session_token = result$session_token
          ))
        }
      } else {
        session$sendCustomMessage("show_login_error", list(
          id = session$ns("error_msg"),
          message = result$message
        ))
      }
    })

    return(shiny::reactive({
      list(
        authenticated = auth_status$authenticated,
        user_info = auth_status$user_info
      )
    }))
  })
}
