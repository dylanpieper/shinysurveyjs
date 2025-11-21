# LDAP Authentication Class

LDAP Authentication Class

LDAP Authentication Class

## Public fields

- `config`:

  LDAP configuration

- `authenticated`:

  Authentication status

- `user_info`:

  User information Initialize LDAP Authentication

## Methods

### Public methods

- [`LdapAuth$new()`](#method-LdapAuth-new)

- [`LdapAuth$ensure_session_table_once()`](#method-LdapAuth-ensure_session_table_once)

- [`LdapAuth$generate_token()`](#method-LdapAuth-generate_token)

- [`LdapAuth$create_session()`](#method-LdapAuth-create_session)

- [`LdapAuth$validate_session()`](#method-LdapAuth-validate_session)

- [`LdapAuth$cleanup_expired_sessions_silent()`](#method-LdapAuth-cleanup_expired_sessions_silent)

- [`LdapAuth$cleanup_expired_sessions()`](#method-LdapAuth-cleanup_expired_sessions)

- [`LdapAuth$logout_session()`](#method-LdapAuth-logout_session)

- [`LdapAuth$get_connection_params()`](#method-LdapAuth-get_connection_params)

- [`LdapAuth$authenticate()`](#method-LdapAuth-authenticate)

- [`LdapAuth$logout()`](#method-LdapAuth-logout)

- [`LdapAuth$clone()`](#method-LdapAuth-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    LdapAuth$new(
      host,
      base_dn,
      port = 389,
      user_attr = "uid",
      domain = NULL,
      ssh_tunnel = NULL,
      db_ops = NULL,
      session_duration_days = 7,
      auth_table = "sjs_auth"
    )

#### Arguments

- `host`:

  LDAP server hostname

- `base_dn`:

  Base Distinguished Name

- `port`:

  LDAP port (default: 389)

- `user_attr`:

  User attribute (default: "uid")

- `domain`:

  Domain for UPN binding (e.g., "pitt.edu")

- `ssh_tunnel`:

  Local port number for SSH tunnel (assumes tunnel already running,
  e.g., `ssh_tunnel = 3389`)

- `db_ops`:

  Database operations object for session management

- `session_duration_days`:

  Number of days sessions remain valid (default: 7)

- `auth_table`:

  Name of authentication sessions table (default: "sjs_auth") Ensure
  session table exists once during initialization (no read check)

------------------------------------------------------------------------

### Method `ensure_session_table_once()`

Creates the authentication session table if it doesn't exist and
performs initial cleanup Generate secure session token

#### Usage

    LdapAuth$ensure_session_table_once()

------------------------------------------------------------------------

### Method `generate_token()`

Generates a 32-byte random hex token for session authentication Create
new session

#### Usage

    LdapAuth$generate_token()

------------------------------------------------------------------------

### Method `create_session()`

#### Usage

    LdapAuth$create_session(username)

#### Arguments

- `username`:

  Username to create session for Validate session token

------------------------------------------------------------------------

### Method `validate_session()`

#### Usage

    LdapAuth$validate_session(token)

#### Arguments

- `token`:

  Session token to validate Clean up expired sessions silently (no
  logging)

------------------------------------------------------------------------

### Method `cleanup_expired_sessions_silent()`

Removes expired sessions from the database without warning messages
Clean up expired sessions

#### Usage

    LdapAuth$cleanup_expired_sessions_silent()

------------------------------------------------------------------------

### Method `cleanup_expired_sessions()`

Removes expired sessions from the database with error logging Remove
specific session

#### Usage

    LdapAuth$cleanup_expired_sessions()

------------------------------------------------------------------------

### Method `logout_session()`

#### Usage

    LdapAuth$logout_session(token = NULL)

#### Arguments

- `token`:

  Session token to remove Get effective host and port (considering SSH
  tunnel)

------------------------------------------------------------------------

### Method `get_connection_params()`

Returns connection parameters, using SSH tunnel settings if configured
Authenticate user credentials

#### Usage

    LdapAuth$get_connection_params()

------------------------------------------------------------------------

### Method `authenticate()`

#### Usage

    LdapAuth$authenticate(username, password, logger = NULL)

#### Arguments

- `username`:

  Username

- `password`:

  Password

- `logger`:

  Optional logger Logout user

------------------------------------------------------------------------

### Method `logout()`

Logs out the current user by removing their session

#### Usage

    LdapAuth$logout()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LdapAuth$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
