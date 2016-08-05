#### send mail script

create_contents_mail <- function(toaddrs,subject,text){
  library(rJython) 
  rJython <- rJython() 
  rJython$exec("import smtplib") 
  rJython$exec("from email.MIMEText import MIMEText") 
  rJython$exec("import email.utils") 
  
  mail <- c(
    "fromaddr = 'davidefloriello.math@gmail.com'", 
    "toaddrs  = 'dav.floriello@gmail.com'", 
    "msg = MIMEText('')", 
    "msg['From'] = email.utils.formataddr(('Admantx process', fromaddr))", 
    "msg['To'] = email.utils.formataddr(('recipient name', toaddrs))", 
    "msg['Subject'] = 'Download tweets'", 
    
    #SMTP server credentials 
    "username = 'davidefloriello.math@gmail.com''", 
    "password = 'armaditaggia'", 
    
    #Set SMTP server and send email, e.g., google mail SMTP server 
    "server = smtplib.SMTP('smtp.gmail.com:587')", 
    "server.ehlo()", 
    "server.starttls()", 
    "server.ehlo()", 
    "server.login(username,password)", 
    "server.sendmail(fromaddr,toaddrs,msg.as_string())", 
    "server.quit()")
  mail[2] <- paste("toaddrs = '",toaddrs,"'")
  mail[3] <- paste("msg = MIMEText('",text,"')")
  mail[6] <- paste("msg['Subject'] = '",subject,"'")
  jython.exec(rJython,mail)
}

create_contents_mail <- function(toaddrs,subject,text){
  library(rJython) 
  rJython <- rJython() 
  rJython$exec("import smtplib") 
  rJython$exec("from email.MIMEText import MIMEText") 
  rJython$exec("import email.utils") 
  
  mail <- c(
    "fromaddr = 'davidefloriello.math@gmail.com' ", 
    "toaddrs  = 'dav.floriello@gmail.com' ", 
    "msg = MIMEText('Ciao bel cipollotto!!!')", 
    "msg['From'] = email.utils.formataddr(('Stefano Barberis', fromaddr))", 
    "msg['To'] = email.utils.formataddr(('recipient name', toaddrs))", 
    "msg['Subject'] = 'Download tweets'", 
    
    #SMTP server credentials 
    "username = 'davidefloriello.math@gmail.com''", 
    "password = 'armaditaggia'",
    
    #Set SMTP server and send email, e.g., google mail SMTP server 
    "server = smtplib.SMTP('smtp.gmail.com:587')", 
    "server.ehlo()", 
    "server.starttls()", 
    "server.ehlo()", 
    "server.login(username,password)", 
    "server.sendmail(fromaddr,toaddrs,msg.as_string())", 
    "server.quit()")
  mail[2] <- paste("toaddrs = '",toaddrs,"'")
  mail[3] <- paste("msg = MIMEText('",text,"')")
  mail[6] <- paste("msg['Subject'] = '",subject,"'")
  jython.exec(rJython,mail)
}
send_mail <- function(step,da){
  subject <- paste("avanzamento creazione dataset")

  text <- paste("step", step, "and day_ahead", da, "done")
  
  create_contents_mail("dav.floriello@gmail.com",subject,text)
  #create_contents_mail("gabriele.tagliabue@smartstat.it",subject,text)
  #create_contents_mail("davide.floriello@smartstat.it", subject, text)
  #create_contents_mail("marco.fattore@smartstat.it",subject,text)
}
################################################################################################
library(mailR)
sender <- "davidefloriello.math@gmail.com"
recipients <- c("dav.floriello@gmail.com")
send.mail(from = sender,
          to = recipients,
          subject = "avanzamento creazione dataset",
          body = "Body of the email",
          smtp = list(host.name = "smtp.gmail.com", port = 465, 
                      user.name = "YOURUSERNAME@gmail.com",            
                      passwd = "YOURPASSWORD", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)