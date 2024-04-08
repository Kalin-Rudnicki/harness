package harness.endpoint.transfer

import zio.*

final case class InputStream(wrapped: java.io.InputStream) {

  // TODO (KR) : do something if  
  def readString: Task[String] = ZIO.attempt { new String(wrapped.readAllBytes()) }

}
