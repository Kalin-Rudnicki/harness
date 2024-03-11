package harness.archive.db.model

import harness.archive.api.model as Api
import harness.sql.*

object LegacyTables {

  object user {

    export harness.archive.db.model.User as V1

  }

  object session {

    export harness.archive.db.model.Session as V1

  }

  object app {

    final case class V1[F[_]](
        id: F[App.Id],
        name: F[String],
        logDurationMap: F[Api.app.DurationMap],
        traceDurationMap: F[Api.app.DurationMap],
    ) extends Table.WithId[F, App.Id]
    object V1 extends Table.Companion.WithId[Api.app.AppId, app.V1] {

      override implicit lazy val tableSchema: TableSchema[app.V1] =
        TableSchema.derived[app.V1]("archive", "app") {
          new app.V1.Cols(
            id = App.Id.pkCol,
            name = Col.string("name"),
            logDurationMap = Col.jsonb[Api.app.DurationMap]("log_duration_map"),
            traceDurationMap = Col.jsonb[Api.app.DurationMap]("trace_duration_map"),
          )
        }

    }

    export harness.archive.db.model.App as V2

  }

  object appToken {

    export harness.archive.db.model.AppToken as V1

  }

  object log {

    export harness.archive.db.model.Log as V1

  }

  object trace {

    export harness.archive.db.model.Trace as V1

  }

}
