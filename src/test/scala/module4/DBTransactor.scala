package module4

import com.dimafeng.testcontainers.PostgreSQLContainer
import com.typesafe.config.Config
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import io.getquill._
import zio.{ZIO, ZLayer}

object DBTransactor {

  type DataSource = javax.sql.DataSource

  object Ctx extends PostgresZioJdbcContext(NamingStrategy(Escape, Literal))

  def hikariDS(config: Config): HikariDataSource = JdbcContextConfig(config).dataSource

  def test: ZLayer[Any/*PostgreSQLContainer*/, Throwable, HikariDataSource] = ZLayer(
    for {
      //pg <- ZIO.service[PostgreSQLContainer]
      config <- ZIO.attempt {
        val hc = new HikariConfig()
        hc.setUsername("fba")
        hc.setPassword("fba")
        hc.setJdbcUrl("jdbc:postgresql://localhost/fba_db")
        hc.setDriverClassName("org.postgresql.Driver")
        hc
      }
      ds <- ZIO.attempt(new HikariDataSource(config))
    } yield ds
  )

}