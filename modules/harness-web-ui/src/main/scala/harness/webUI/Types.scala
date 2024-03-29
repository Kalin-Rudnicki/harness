package harness.webUI

import harness.http.client.*
import harness.webUI.error.UIError
import harness.zio.*
import zio.*

type PageTask[+A] = ZIO[HarnessEnv & HttpClient.ClientT, UIError.Failure, A]
type PageLoadTask[+A] = ZIO[HarnessEnv & HttpClient.ClientT, UIError, A]
