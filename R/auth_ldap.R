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
    initialize = function(host, base_dn, port = 389, user_attr = "uid", domain = NULL, ssh_tunnel = NULL) {
      self$config <- list(
        host = host,
        base_dn = base_dn,
        port = port,
        user_attr = user_attr,
        domain = domain,
        ssh_tunnel = ssh_tunnel
      )

      # Set up SSH tunnel if configured (simple local case only)
      if (!is.null(ssh_tunnel) && is.numeric(ssh_tunnel)) {
        self$config$ssh_tunnel <- list(enabled = TRUE, local_port = ssh_tunnel)
        message("SSH tunnel configured for localhost:", ssh_tunnel)
      }
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

          if (!is.null(logger)) {
            logger$log_message(paste("Original username:", username), type = "DEBUG", zone = "AUTH")
            logger$log_message(paste("Clean username:", clean_username), type = "DEBUG", zone = "AUTH")
            logger$log_message(paste("Final bind username:", bind_username), type = "DEBUG", zone = "AUTH")
            logger$log_message(paste("Domain config:", self$config$domain), type = "DEBUG", zone = "AUTH")
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
            self$user_info <- list(
              username = username,
              login_time = Sys.time()
            )

            if (!is.null(logger)) {
              logger$log_message(paste("LDAP auth successful:", username), type = "INFO", zone = "AUTH")
            }

            return(list(success = TRUE, user_info = self$user_info))
          } else {
            if (!is.null(logger)) {
              logger$log_message(paste("LDAP auth failed:", username), type = "WARN", zone = "AUTH")
            }
            return(list(success = FALSE, message = "Invalid credentials"))
          }
        },
        error = function(e) {
          if (!is.null(logger)) {
            logger$log_message(paste("LDAP error:", e$message), "ERROR", "AUTH")
          }
          return(list(success = FALSE, message = paste("Authentication error:", e$message)))
        }
      )
    },


    #' Logout user
    logout = function() {
      self$authenticated <- FALSE
      self$user_info <- NULL
    }
  )
)

#' Create LDAP login UI
#' @param id Namespace ID
#' @param title Login form title
#' @param logo Logo URL to display instead of title (optional)
#' @export
ldap_login_ui <- function(id, title = "Login", logo = NULL) {
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
      style = "width: 100%; padding: 12px; font-size: 16px; font-weight: 500; background-color: #007bff; border-color: #007bff; border-radius: 4px; transition: all 0.15s ease-in-out;"
    ),

    # JavaScript for better UX
    shiny::tags$script(shiny::HTML(sprintf('
      $(document).ready(function() {
        var ns_prefix = "%s";

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
          $(this).css("border-color", "#007bff");
          $(this).css("box-shadow", "0 0 0 0.2rem rgba(0, 123, 255, 0.25)");
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

      // Custom message handler for showing errors
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
    ', ns(""))))
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

    shiny::observeEvent(input$login_btn, {
      shinyjs::hide("error_msg")

      result <- ldap_auth$authenticate(input$username, input$password, logger)

      if (result$success) {
        auth_status$authenticated <- TRUE
        auth_status$user_info <- result$user_info
        shinyjs::hide("login_form")
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
