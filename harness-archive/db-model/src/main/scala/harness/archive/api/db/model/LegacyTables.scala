package harness.archive.api.db.model

import harness.archive.model as D
import harness.sql.*

object LegacyTables {

  object user {

    export harness.archive.api.db.model.User as V1

  }

  object session {

    export harness.archive.api.db.model.Session as V1

  }

  object app {

    final case class V1[F[_]](
        id: F[App.Id],
        name: F[String],
        logDurationMap: F[D.app.DurationMap],
        traceDurationMap: F[D.app.DurationMap],
    ) extends Table.WithId[F, App.Id]
    object V1 extends Table.Companion.WithId[D.app.AppId, app.V1] {

      override implicit lazy val tableSchema: TableSchema[app.V1] =
        TableSchema.derived[app.V1]("archive", "app") {
          new app.V1.Cols(
            id = App.Id.pkCol,
            name = Col.string("name"),
            logDurationMap = Col.jsonb[D.app.DurationMap]("log_duration_map"),
            traceDurationMap = Col.jsonb[D.app.DurationMap]("trace_duration_map"),
          )
        }

    }

    export harness.archive.api.db.model.App as V2

  }

  object appToken {

    export harness.archive.api.db.model.AppToken as V1

  }

  object log {

    export harness.archive.api.db.model.Log as V1

  }

  object trace {

    export harness.archive.api.db.model.Trace as V1

  }

}
