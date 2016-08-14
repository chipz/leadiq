package models

import org.specs2.mock.Mockito
import org.specs2.specification.Scope
import org.specs2.mutable._


class TransactionModelSpec extends Specification with Mockito {

  trait scoping extends Scope {
    val amountT1 = 20
    val t1 = Transaction(1, amountT1, "car", None)
    val amountT2 = 50
    val t2 = Transaction(2, amountT2, "car", None)
    val amountT3 = 30
    val t3 = Transaction(3, amountT3, "car", Some(t2.id))
    val amountT4 = 40
    val t4 = Transaction(4, amountT4, "car", Some(t3.id))
    TransactionModel.insert(t1, None)
    TransactionModel.insert(t2, None)
    TransactionModel.insert(t3, Some(2))
    TransactionModel.insert(t4, Some(3))

    val amountT5 = 50
    val t5 = Transaction(5, amountT5, "gas", None)
    val amountT6 = 60
    val t6 = Transaction(6, amountT6, "gas", None)
    TransactionModel.insert(t5, None)
    TransactionModel.insert(t6, None)
  }

  "Transaction Model" should {
    "return a transaction" in new scoping {
      TransactionModel.get(1) must beSome(t1)
    }

    "return none when transaction not exist" in new scoping {
      TransactionModel.get(100) must beEqualTo(None)
    }

    "must return its on value of a childless transaction" in new scoping {
      TransactionModel.sum(t1.id) must beEqualTo(amountT1)
    }

    "must return a sum of all transactions that are transitively linked by their parent_id" in new scoping {
      TransactionModel.sum(3) must beEqualTo(amountT3 + amountT4)
    }

    "must return a sum of all transactions that are transitively linked by their parent_id to back to the root" in new scoping {
      TransactionModel.sum(2) must beEqualTo(amountT2 + amountT3 + amountT4)
    }

    "must return all value of the same type" in new scoping {
      TransactionModel.getByType("gas") must beEqualTo(List(t5, t6))
    }

    "must return empty parent if parent not exist" in {
      val notExistParentID = Some(200L)
      val tx = Transaction(100, 100, "nope", notExistParentID)
      TransactionModel.insert(tx, notExistParentID)
      TransactionModel.get(tx.id) must beSome(tx.copy(parentID = None))
    }

    "cannot insert transaction with the same id" in new scoping {
      TransactionModel.get(1) must beSome(t1)
      val ty = t1.copy(amount = 200)
      TransactionModel.insert(ty, None)
      TransactionModel.get(1) must beSome(t1)
    }
  }

}
