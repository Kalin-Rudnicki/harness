package harness.http.server

import zio.Scope

type BuiltInRequestEnv = HttpRequest & Scope
