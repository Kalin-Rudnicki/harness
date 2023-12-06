package harness.email

import javax.mail.Address as JavaAddress
import javax.mail.internet.InternetAddress as JavaInternetAddress

implicit class EmailAddressOps(self: EmailAddress) {
  def toJava: JavaAddress = new JavaInternetAddress(self.unwrap)
}
