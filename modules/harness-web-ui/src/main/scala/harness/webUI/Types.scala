package harness.webUI

import harness.webUI.error.UIError
import zio.*

type PageTask[+A] = IO[UIError.Failure, A]
type PageLoadTask[+A] = IO[UIError, A]
