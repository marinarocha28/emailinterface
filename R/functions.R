#' Check if an object has class try-error
#'
#' @param x Any object#'
#' @return FALSE or TRUE
#' @export

is.error = function (x) {inherits(x, "try-error")}

#' Clean the email string to save it as a valid keyring name
#'
#' @param x a string
#' @return string without . or - or \@

limpa_string = function(x) {
  x %>%
    gsub(pattern = ('.'), replacement = '', fixed = TRUE) %>%
    gsub(pattern = ('@'), replacement = '', fixed = TRUE) %>%
    gsub(pattern = ('-'), replacement = '', fixed = TRUE) %>%
    tolower()
}

# emails and passwords ---------------------------------------------------------

#' Register the email in a keyring for future use
#'
#' @param email The email adress.
#'
#' @return NULL
#'
#' @details This function creates a keyring with your email and additional parameters (password, email provider, etc)
#' used in the `credentials` parameters of blastula `smtp_send`. Some dialogs will pop asking for your email
#' password, a PIN code used to retrieve this password and so on.
#' @export

register_email_address = function(email) {
  keyrings = keyring::keyring_list()$keyring
  keyring_name = email %>% limpa_string()

  if (keyring_name %in% keyrings) {
    temp = 'Email address already registered. Do you want to delete it and insert it again?' %>% svDialogs::dlg_message(type = 'yesno')

    if (temp$res %in% 'no') {
      return(NULL)
    } else {
      keyring::keyring_delete(keyring_name)
    }
  }

  'In the next screen, insert a PIN code. Something easy to unlock your keyring when you send emails.' %>% svDialogs::msgBox()
  keyring::keyring_create(keyring_name)

  'In the next screen, insert your email password' %>% svDialogs::msgBox()
  keyring::key_set(service = 'password', keyring = keyring_name)

  temp = svDialogs::dlgList(title = 'Select the provider', choices = c('gmail', 'outlook', 'office365', 'other'), rstudio = FALSE)
  provider = temp$res
  keyring::key_set_with_value(service = 'provider', keyring = keyring_name, password = provider)

  if (provider %in% 'other') {
    temp = svDialogs::dlg_input(message = 'Type the SMTP host of the email provider', default = '')
    host = temp$res
    keyring::key_set_with_value(service = 'host', keyring = keyring_name, password = host)

    temp = svDialogs::dlg_input(message = 'Type the SMTP port of the email provider', default = '')
    port = temp$res
    keyring::key_set_with_value(service = 'port', keyring = keyring_name, password = port)
  }

  keyring::keyring_lock(keyring_name)

  return(NULL)

}

#' Internal function to create the credentials file used in blastula
#'
#' @param email The email adress.
#'
#' @return A credentials file
#'
#' @export
#'
get_email_credentials = function(email, file_name = NULL) {
  keyring_name = email %>% limpa_string()

  if (!is.null(file_name)) {
    credentials = blastula::creds_file(file = file_name)
    return(credentials)
  }

  # if you cant find the email on the keyring, just ask for the password
  if (!keyring_name %in% keyring::keyring_list()$keyring) {
    'Email account not registered!' %>% print()
    credentials =
      blastula::creds(
        provider = "office365",
        user = email
      )

    return(credentials)
    # register_email_address(keyring_name)
  }

  # otherwise, check the password on the keyring
  password =
    try(expr = {keyring::key_get(service = 'password', keyring = keyring_name)}, silent = TRUE)

  if (password %>% is.error()) {
    warning('Incorrect PIN code!')
    return(NULL)
  }

  # set the password to the env so the creds_envvar function can get it
  Sys.setenv('email_password' = password)

  provider = keyring::key_get(service = 'provider', keyring = keyring_name)
  if (!provider %in% 'other') {
    credentials = blastula::creds_envvar(user = email, pass_envvar = 'email_password', provider = provider)
  } else {
    host = keyring::key_get(service = 'host', keyring = keyring_name)
    port = keyring::key_get(service = 'port', keyring = keyring_name)

    credentials = blastula::creds_envvar(user = email, pass_envvar = 'email_password', provider = provider, host = host, port = port, use_ssl = TRUE)
  }

  Sys.unsetenv('email_password')
  keyring::keyring_lock(keyring_name)

  return(credentials)
}

# template ----------------------------------------------------------------

#' An email template
#'
#' @param from Your email
#' @param to A vector of emails
#' @param cc A vector of emails
#' @param bcc A vector of emails
#' @param subject A string with the subject
#' @param title The title of the email (see `blastula`)
#' @param body Where the main text goes
#' @param header A string with the header text
#' @param footer A string with the footer text
#' @param attachments A vector with attachments names (local paths or absolute paths)
#'
#' @return A named list used to create the email
#'
#' @export
email_template = function(from = NULL, to = NULL, cc = NULL, bcc = NULL,
                          subject = NULL, title = NULL, body = NULL,
                          header = NULL, footer = NULL, attachments = NULL) {
  list(
    from = from
    ,to = to
    ,cc = cc
    ,bcc = bcc
    ,subject = subject
    ,title = title
    ,body = body
    ,header = header
    ,footer = footer
    ,attachments = attachments
  )

}

# preparing to blastula ---------------------------------------------------

#' Preparing the email
#'
#' @param emails_args A list as created by `email_template`
#'
#' @return An object suited to be passed to `blastula`
#'
#' @export

prepare_email = function(email_args) {

  email_object =
    blastula::compose_email(
      body = email_args$body
      ,header = email_args$header
      ,footer = email_args$footer
      ,title = email_args$title
    )

  for (i in seq_along(email_args$attachments)) {
    email_object %<>%
      blastula::add_attachment(file = email_args$attachments[i])
  }

  email_object

}

# sending -----------------------------------------------------------------

#' Sends the emails
#'
#' @param email_args_list A list as created by `email_template` or a list of those lists.
#' @param credentials Used to authenticate blastula. If NULL, it gets a credential object from `get_email_credentials`.
#'
#' @export


send_email = function(email_args_list, credentials = NULL, .function = NULL, file_name = NULL, ...) {

  if (is.list(email_args_list[[1]]) == FALSE) {
    email_args_list = list(email_args_list)
  }

  email = email_args_list[[1]]$from
  keyring_name = email %>% limpa_string()

  # on.exit(expr = {Sys.unsetenv('email_password')}, add = TRUE)

  if (is.null(credentials)) {
    credentials = get_email_credentials(email, file_name)
  }

  i = 1
  for (i in seq_along(email_args_list)) {

    email_args = email_args_list[[i]]
    email_object =
      email_args %>%
      prepare_email()

    paste('Sending email', i, '...') %>% message()

    email_object %>%
      blastula::smtp_send(
        from = email_args$from,
        to = email_args$to,
        cc = email_args$cc,
        bcc = email_args$bcc,
        subject = email_args$subject,
        credentials = credentials
      )
  }

  if (!is.null(.function)) {
    .function(...)
  }

  return(TRUE)

}

# modules -----------------------------------------------------------------

#' UI module
#'
#' @export
md.emailinterface_UI <- function(id = 'email', email_args_list) {
  ns <- NS(id)

  if (is.list(email_args_list[[1]]) == FALSE) {
    email_args_list = list(email_args_list)
  }

  n_email = length(email_args_list)

  email_args = email_args_list[[1]]

  email_objects =
    email_args_list %>%
    lapply(prepare_email)

  ui =
    tagList(
      shiny::fluidRow(
        shinydashboard::box(
          width = 12
          ,shiny::column(
            6
            ,shinyWidgets::actionBttn(
              inputId = ns('previous_email'),
              label = 'Previous', block = TRUE, icon = shiny::icon('angle-left'), style = "fill"
            )
          )
          ,shiny::column(
            6
            ,shinyWidgets::actionBttn(
              inputId = ns('next_email'),
              label = 'Next', block = TRUE, icon = shiny::icon('angle-right'), style = "fill"
            )
          )
        )
      )

      ,shiny::fluidRow(
        shinydashboard::box(
          width = 12
          ,shiny::uiOutput(outputId = ns('email_body'))
        )
      )

      ,shiny::fluidRow(
        shinydashboard::box(
          width = 12
          ,shiny::column(
            6
            ,shinyWidgets::actionBttn(
              inputId = ns('send_one'), label = 'Send one', block = TRUE,
              style = "fill", icon = shiny::icon('envelope')
            )
          )
          ,shiny::column(
            6
            ,shinyWidgets::actionBttn(
              inputId = ns('send_all'), label = 'Send all', block = TRUE,
              style = "fill", icon = shiny::icon('mail-bulk'))
          )
        )
      )
    )

  return(ui)
}

#' Server module
#'
#' @export
#'
md.emailinterface <- function(id = 'email', email_args_list, file_name = NULL) {
  moduleServer(
    id
    ,function(input, output, session) {

      if (is.list(email_args_list[[1]]) == FALSE) {
        email_args_list = list(email_args_list)
      }

      n_email = length(email_args_list)

      email_args = email_args_list[[1]]

      email_objects =
        email_args_list %>%
        lapply(prepare_email)

      rv.selected_email_id = shiny::reactiveVal(1)
      rv.status = shiny::reactiveValues(tb = dplyr::tibble(id = seq_along(email_args_list), status = 'Not sent!'))

      # output$email_body -------------------------------------------------------
      output$email_body = shiny::renderUI({

        shiny::req(rv.selected_email_id)

        id = rv.selected_email_id()

        email_args = email_args_list[[id]]
        email_object = email_objects[[id]]

        # browser()

        tagList(
          fluidRow(
            strong('From: ')
            ,email_args$from %>% paste(collapse = ',')
          )

          ,fluidRow(
            strong('To: ')
            ,email_args$to %>% paste(collapse = ',')
          )

          ,if (!is.null(email_args$cc)) {
            fluidRow(
              strong('cc: ')
              ,email_args$cc %>% paste(collapse = ',')
            )
          }

          ,if (!is.null(email_args$bcc)) {
            fluidRow(
              strong('bcc: ')
              ,email_args$bcc %>% paste(collapse = ',')
            )
          }

          ,fluidRow(
            strong('Subject: ')
            ,email_args$subject %>% paste(collapse = ',')
          )

          ,if (!is.null(email_args$attachments)) {
            tagList(
              strong('Attachments: ')
              ,paste(
                email_args$attachments
                ,email_args$attachments %>% file.size() %>% `/`(1024^2) %>% round(2) %>% paste0('mb')
              ) %>% paste(collapse = ',')

              ,strong('Total size: ')
              ,email_args$attachments %>% file.size() %>% `/`(1024^2) %>% sum(na.rm = TRUE) %>% round(2) %>% paste0('mb')
            )
          }

          ,fluidRow(
            email_object$'html_html'
          )

          ,fluidRow(
            'Status: '
            ,rv.status$tb$status[id] %>% as.character()
          )
        )
      })

      observe({
        id = rv.selected_email_id()
        req(id)
        req(id < n_email)

        rv.selected_email_id(id + 1)
      }) %>%
        bindEvent(input$next_email)

      observe({
        id = rv.selected_email_id()
        req(id)
        req(id > 1)

        rv.selected_email_id(id - 1)
      }) %>%
        bindEvent(input$previous_email)

      # rc.credentials ----------------------------------------------------------
      rc.credentials = reactive({

        credentials = get_email_credentials(email = email_args_list[[1]]$from, file_name = file_name)

      })

      observe({

        # browser()

        id = rv.selected_email_id()
        req(id)

        email_args = email_args_list[[id]]

        paste('Sending email', id, '...') %>% showNotification()

        email_args %>%
          send_email(credentials = rc.credentials())

        rv.status$tb %<>%
          rows_update(tibble(id = id, status = 'Sent!'))

        'Done!' %>% showNotification()

      }) %>%
        bindEvent(input$send_one)

      observe({
        'Sending all emails...' %>% showNotification()

        id_not_sent =
          rv.status$tb %>%
          filter(status == 'Not sent!') %>%
          pull(id)

        req(id_not_sent)

        email_args_list %>%
          .[id_not_sent] %>%
          send_email(credentials = rc.credentials())

        'Done!' %>% showNotification(duration = 600)

        rv.status$tb$status = 'Sent!'

      }) %>%
        bindEvent(input$send_all)

    })
}


# interface  -------------------------------------------------------
#' Email interface
#'
#' @param email_args_list A list as created by `email_template` or a list of those lists.
#' @param viewer One of dialog, browser or pane. Where the shinyapp will be executed.
#' @param height The height when viewer is set to dialog.
#' @param width The width when viewer is set to dialog.
#'
#' @details A simple shiny app that lets you see the details of the emails you composed. You can
#' browser the emails and send one or more. A sent email can't be sent again. If you sent one or more
#' and then click in 'Send all', it will send only the ones that were not sent yet.
#'
#' @export

email_interface = function(email_args_list, viewer = 'dialog', height = 800, width = 800, file_name = NULL) {

  if (viewer %in% 'dialog') {
    viewer = shiny::dialogViewer(dialogName = "email_inteRface", width = width, height = height)
  } else if (viewer %in% 'browser') {
    viewer = shiny::browserViewer()
  } else if (viewer %in% 'pane') {
    viewer = shiny::paneViewer()
  } else {
    stop('Pass a valid viewer! Options are dialog, browser, pane.')
  }


  ui <-
    shinydashboard::dashboardPage(
      header = shinydashboard::dashboardHeader(disable = TRUE)
      ,sidebar = shinydashboard::dashboardSidebar(disable = TRUE)
      ,body = shinydashboard::dashboardBody(
        md.emailinterface_UI(email_args_list = email_args_list)
      )
    )

  server <- function(input, output, session) {

    md.emailinterface(email_args_list = email_args_list, file_name = file_name)

  }

  runGadget(ui, server, viewer = viewer)

}
