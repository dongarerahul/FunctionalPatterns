import java.util.{Calendar, Date}

import Common.{Account, Balance}

import util.{Failure, Success, Try}
import scala.util.Try

object Common {
  type Amount = BigDecimal

  def today = Calendar.getInstance.getTime

  sealed trait Currency
  case class Balance(amount: Amount = 0)
  case class Money(amount: BigDecimal)
  case class Position(account: Account, currency: Currency, balance: Money)

  case class Address(no: String, street: String, city: String, state: String, zip: String)
  case class Customer(id: Int, name: String, address: Address)

  sealed trait Account {
    def no: String

    def name: String

    def dateOfOpen: Option[Date]

    def dateOfClose: Option[Date]

    def balance: Balance
  }

  case class Lens[O, V] (get: O => V, set: (O, V) => O)

  def compose[Outer, Inner, Value](outer: Lens[Outer, Inner], inner: Lens[Inner, Value]) =
    Lens[Outer, Value](
      get = outer.get andThen inner.get,
      set = (obj: Outer, value: Value) => outer.set(obj, inner.set(outer.get(obj), value))
    )
}

trait AccountService[Account, Amount, Balance] {
  def open(name: String, no: String, openingDate: Option[Date]): Try[Account]

  def open(account: Account, closingDate: Option[Date], repo: AccountRepository): Try[Account]

  def credit(account: Account, amount: Amount): (AccountRepository => Try[Account])

  def debit(account: Account, amount: Amount): (AccountRepository => Try[Account])

  def balance(account: Account, amount: Amount): (AccountRepository => Try[Balance])

  def transfer(from: Account, to: Account, amount: Amount): (AccountRepository => Try[(Account, Account, Amount)]) = {
    for {
      a <- debit(from, amount)
      b <- credit(to, amount)
    } yield (a, b, amount)
  }
}

sealed trait Repository [Account, String]

trait AccountRepository extends Repository[Account, String] {
  def query(accountNo: String) : Try[Option[Account]]
  def store(a: Account) : Try[Account]
  def balance(accountNo: String) : Try[Balance]
  def openedOn(date: Date): Try[Seq[Account]]
}

import Common._

final case class CheckingAccount private(no: String,
                                         name: String,
                                         dateOfOpen: Option[Date],
                                         dateOfClose: Option[Date],
                                         balance: Balance) extends Account

final case class SavingsAccount private(no: String,
                                         name: String,
                                         dateOfOpen: Option[Date],
                                         dateOfClose: Option[Date],
                                         balance: Balance) extends Account

object Account {
  private def closeDataCheck(openDate: Option[Date], closeDate: Option[Date]) : Try[(Date, Option[Date])] = {
    val od = openDate.getOrElse(today)
    closeDate.map(cd =>
      if(cd before od)
        Failure(new Exception("CD > OD"))
      else
        Success(od, closeDate)
    ).getOrElse(Success(od, closeDate))
  }

  /*def checkingAccount(no: String,
                      name: String,
                      dateOfOpen: Option[Date],
                      dateOfClose: Option[Date],
                      balance: Balance) : Try[Account] = {

  }*/
}

object Main {

  def main(args: Array[String]): Unit = {
    val a = Address(no = "B-12", street = "My Street", city = "MyCity", state = "MH", zip = "411043")
    val c = Customer(12, "RD", a)

    val addressNoLens = Lens[Address, String](
      get = _.no,
      set = (o, v) => o.copy(no = v)
    )

    val custAddressLens = Lens[Customer, Address](
      get = _.address,
      set = (o, v) => o.copy(address = v)
    )

    val custAddressNoLens = compose(custAddressLens, addressNoLens)
    val c1 = custAddressNoLens.set(c, "B675")

    println("Existing Customer: " + custAddressNoLens.get(c))
    println("Updated Customer : " + custAddressNoLens.get(c1))
  }
}