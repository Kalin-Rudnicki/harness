//

val Scala_3 = "3.1.2-RC3"

val MyOrg = "io.github.kalin-rudnicki"

// =====| Shared Settings |=====

// =====| Projects |=====

lazy val `harness-root` =
  project
    .in(file("."))
    .settings(
      publish / skip := true,
    )
    .aggregate(
    )
