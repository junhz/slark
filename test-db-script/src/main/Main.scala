package main

import slark.db.builder._
import oracle.jdbc.driver.OracleDriver

object Main {

  def main(args: Array[String]): Unit = {
    val sqls = Sqls(new OracleDriver)
    import sqls._

    val prod = connect("jdbc:oracle:thin:@dcmn02:1527:O01CMN3", "a554114", "Apr2014")
    val cmtc74 = connect("jdbc:oracle:thin:@10.248.98.74:1521:CMTC74", "CMNPID", "Mar2009")

    val r = for (
      rs1 <- query"""
select 
  rpt_sched_id,
  rpt_sched_description, 
  rpt_sched_cycle, 
  rpt_sched_day, 
  rpt_sched_time, 
  rpt_sched_date_period, 
  rpt_sched_after_sec_eod, 
  rpt_sched_after_cash_eod,
  rpt_export_id, 
  rpt_sched_user_id
from RPT_SCHEDULE""" prepare prod;
      (id, desc, cycle, day, time, period, sec, cash, export, creator) <- rs1;
      _ <- update"""
insert into RPT_SCHEDULE (
  rpt_sched_id,
  rpt_sched_description, 
  rpt_sched_cycle, 
  rpt_sched_day, 
  rpt_sched_time, 
  rpt_sched_date_period, 
  rpt_sched_after_sec_eod, 
  rpt_sched_after_cash_eod,
  rpt_export_id, 
  rpt_sched_user_id) 
values (
  $id, $desc, $cycle, $day, $time, $period, $sec, $cash, $export, ${if (creator.toString.endsWith(" process")) creator else "rains"})""" prepare cmtc74
    ) yield {


      val queryParameterID = query"""select rpt_param_type_id from RPT_SCHEDULE_PARAMETER where rpt_schedule_id = $id""" prepare prod

      val queryRequestID = query"""select max(rpt_request_id) from RPT_REQUEST 
    where rpt_export_id = $export and rpt_request_status_id in ('DONE', 'NULL')""" prepare cmtc74

      (for (rs2 <- queryParameterID; rs3 <- queryRequestID) yield for (pID <- rs2; rID <- rs3) yield {
        (update"""insert into RPT_SCHEDULE_PARAMETER (rpt_schedule_id, rpt_param_type_id, rpt_schedule_param_value) values 
    ($id, $pID, (select rpt_request_param_value from RPT_REQUEST_PARAMETER where rpt_request_id = $rID and rpt_param_type_id = $pID))""" prepare cmtc74).execute
      })
    }
    r.execute match {
      case Succ(l) => {
        println(l)
        cmtc74.commit
      }
      case Fail(msg) => {
        println(msg)
      }
    }

    prod.close
    cmtc74.close
  }

}