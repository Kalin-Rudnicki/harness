package harness.web.server

import zio.Scope

type ServerEnv = Any
type RequestEnv = HttpRequest & Scope
