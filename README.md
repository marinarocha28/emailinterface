# emailinterface

A simple interface to send emails using R. Made on top of blastula and keyring.

## Instalation

Install it with 
```
# install.packages("remotes")
remotes::install_github('marinarocha28/emailinterface')
```

## Usage

Use `register_email_address` to register your email address, password and additional parameters needed to the SMTP protocol (they are automatically set
if your email is from Google, Outlook or Office365). When using Gmail, you need to activate the option *less secure apps* on your account.

Retrieve your password using a PIN code so you don't need to type the entire password every time.

## Interface

A shiny app will display your prepared emails with informations about to, cc, bcc, attachments and the body of the email. Navigate 
throught the emails, send one at a time or all at once.

## Example

```
# install.packages("remotes")
remotes::install_github('marinarocha28/emailinterface')

library(emailinterface)

email = 'marinarocha2001@hotmail.com'

register_email_address(email)
```

After inserting the PIN code, the password and remaining informations, let's create some sample email messages:

```
example_emails = 
  1:5 %>% 
  lapply(function(i) {
    email_template(
      from = email
      ,to = 'marinarocha2001@hotmail.com'
      ,subject = paste('Test', i)
      ,body = 'Nice example text!!'
      ,footer = 'Sent via the marvelous emailinterface package!!'
    )
  })
```

and run the interface

```
example_emails %>% 
  email_interface()
```

![](./man/interface_example.png)
