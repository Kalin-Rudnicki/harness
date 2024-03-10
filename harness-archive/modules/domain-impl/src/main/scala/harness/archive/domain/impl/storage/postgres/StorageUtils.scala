package harness.archive.domain.impl.storage.postgres

import harness.sql.error.QueryError
import harness.zio.ErrorMapper
import harness.archive.domain.model.DomainError

object StorageUtils {

  implicit val errorMapper: ErrorMapper[QueryError, DomainError] =
    DomainError.UnexpectedStorageError(_)

}
