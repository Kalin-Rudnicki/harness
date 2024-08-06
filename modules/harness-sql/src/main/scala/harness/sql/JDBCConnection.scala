package harness.sql

import java.util.UUID

private[sql] final case class JDBCConnection(jdbcConnection: java.sql.Connection, id: UUID)
