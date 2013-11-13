package models.util

import scala.slick.lifted.{TypeMapper, MappedTypeMapper}
import org.joda.time.{LocalDate, DateMidnight}
import java.sql.Date
import securesocial.core._
import securesocial.core.OAuth2Info
import securesocial.core.OAuth1Info
import securesocial.core.PasswordInfo
import scala.Some

object Mappers {

  implicit def date2dateTime = MappedTypeMapper.base[LocalDate, Date](
    dateTime => new Date(dateTime.toDate.getTime),
    date => new LocalDate(date)
  )

  implicit def tuple2UserId(tuple: (String, String)) = tuple match {
    case (userId, providerId) => IdentityId(userId, providerId)
  }

  implicit def string2AuthenticationMethod: TypeMapper[AuthenticationMethod] =
    MappedTypeMapper.base[AuthenticationMethod, String](
      authenticationMethod => authenticationMethod.method,
      string => AuthenticationMethod(string)
    )

  implicit def tuple2OAuth1Info(tuple: (Option[String], Option[String])) =
    tuple match {
      case (Some(token), Some(secret)) => Some(OAuth1Info(token, secret))
      case _ => None
    }

  implicit def tuple2OAuth2Info(tuple: (Option[String], Option[String],
    Option[Int], Option[String])) = tuple match {
    case (Some(token), tokenType, expiresIn, refreshToken) =>
      Some(OAuth2Info(token, tokenType, expiresIn, refreshToken))
    case _ => None
  }

  implicit def tuple2PasswordInfo(tuple: (String, String, Option[String])) =
    tuple match {
      case (hasher, password, salt) => Some(PasswordInfo(hasher, password, salt))
      case _ => None
    }
}
