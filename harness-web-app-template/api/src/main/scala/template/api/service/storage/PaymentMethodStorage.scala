package template.api.service.storage

import harness.payments.model.ids.*
import harness.sql.*
import harness.sql.query.{given, *}
import harness.zio.*
import template.api.db.model.User
import template.api.db.model as M
import template.model as D
import zio.*

trait PaymentMethodStorage {
  def insert(paymentMethod: M.PaymentMethod.Identity): HRIO[Logger & Telemetry, Unit]
  def getById(id: D.paymentMethod.PaymentMethodId): HRIO[Logger & Telemetry, M.PaymentMethod.Identity]
  def getForUser(userId: D.user.UserId): HRIO[Logger & Telemetry, Chunk[M.PaymentMethod.Identity]]
}
object PaymentMethodStorage {

  // =====| API |=====

  def insert(paymentMethod: M.PaymentMethod.Identity): HRIO[PaymentMethodStorage & Logger & Telemetry, Unit] =
    ZIO.serviceWithZIO[PaymentMethodStorage](_.insert(paymentMethod))

  def getById(id: D.paymentMethod.PaymentMethodId): HRIO[PaymentMethodStorage & Logger & Telemetry, M.PaymentMethod.Identity] =
    ZIO.serviceWithZIO[PaymentMethodStorage](_.getById(id))

  def getForUser(userId: D.user.UserId): HRIO[PaymentMethodStorage & Logger & Telemetry, Chunk[M.PaymentMethod.Identity]] =
    ZIO.serviceWithZIO[PaymentMethodStorage](_.getForUser(userId))

  // =====| Live |=====

  val liveLayer: URLayer[JDBCConnection, PaymentMethodStorage] =
    ZLayer.fromFunction(new Live(_))

  final class Live(con: JDBCConnection) extends PaymentMethodStorage {

    override def insert(paymentMethod: M.PaymentMethod.Identity): HRIO[Logger & Telemetry, Unit] =
      con.use { Q.insert(paymentMethod).single }

    override def getById(id: D.paymentMethod.PaymentMethodId): HRIO[Logger & Telemetry, M.PaymentMethod.Identity] =
      con.use { Q.selectById(id).single(s"Invalid payment-method id '$id'") }

    override def getForUser(userId: D.user.UserId): HRIO[Logger & Telemetry, Chunk[M.PaymentMethod.Identity]] =
      con.use { Q.forUser(userId).chunk }

    // =====| Queries |=====

    private object Q extends TableQueries[M.PaymentMethod.Id, M.PaymentMethod] {

      val forUser: QueryIO[D.user.UserId, M.PaymentMethod.Identity] =
        Prepare.selectIO("PaymentMethodStorage - forUser") { Input[D.user.UserId] } { userId =>
          Select
            .from[M.PaymentMethod]("pm")
            .where { pm => pm.userId === userId }
            .returning { pm => pm }
        }

    }

  }

}
