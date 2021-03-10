
#' Import TSTATS from CAD
#'
#' Requires network connection to CAD reporting database.
#'
#' @param tzone Lubridate time zone to target dates/times to. DB stores them in UTC, default target is "US/Central"
#' @param end_date end date (defaults to last month end)
#' @param start_date start date (defaults to last month start)
#'
#' @return
#' @export
#'
get_cad_incidents <- function(agency, end_date = FALSE, start_date = FALSE, tzone="US/Central") {
  # Set dates: End is last second of last month, start is a year prior

  if (!end_date) {
    end_date <- force_tz(floor_date(today(), unit="month") - seconds(1), tzone=tzone)
  }
  if (!start_date) {
    start_date <- end_date - years(1) + seconds(1)
  }

  #Correct time zone for query dates
  end_date <- with_tz(end_date, tzone="UTC")
  start_date <- with_tz(start_date, tzone="UTC")


  # CREATE CONNECTION
  con <- dbConnect(odbc::odbc(),
                   .connection_string = 'driver={SQL Server};server=cadsql3;database=CypressCreekCAD;trusted_connection=true')

  # add Time Zone correction
  firstDate <- format(start_date, format="%D %T")
  lastDate <- format(end_date, format="%D %T")
  queryStatement = paste0("SELECT fi.IncidentNumber as IncidentNumber,
                          fi.CreatedDate as CreatedDate,
                          fi.CreatedDate as DateTimeasText,
                          n.NatureCode as NatureCode,
                          n.[Description] as NatureDescription,
                          il.LocationAddress as IncidentLocation,
                          cagent.Identifier as CallFDID,
                          c.ReceivedDate as ReceivedDate,
                          j.Description as FireJurisdiction,
                          u.UnitName as UnitName,
                          agent.Identifier as UnitFDID,
                          fi.ShippedDate as ShippedTime,
                          fi.FirstDispatchDate as AlarmTime,
                          afl.OnSceneTime as OnSceneTime,
                          afl.DispatchedTime as DispatchedTime,
                          afl.EnrouteTime as EnrouteTime,
                          afl.StagedTime as StagedTime,
                          afl.EnrouteToHospitalTime as TransportTime,
                          afl.AtHospitalTime as AtHospitalTime,
                          afl.AvailableTime as AvailableTime,
                          area.AreaCode as FireArea,
                          zone.ZoneCode as LocationZone,
                          ut.UnitTypeCode as UnitType,
                          fs.StationNumber as FireStation,
                          cc.Code as CallCategory,
                          c.CallBackNumber as CallbackNumber,
                          firsttimes.OnSceneTime as FirstOnSceneTime,
                          firsttimes.DispatchedTime as FirstDispatchedTime,
                          firsttimes.EnrouteTime as FirstEnrouteTime,
                          firsttimes.StagedTime as FirstStagedTime,
                          firsttimes.EnrouteToHospitalTime as FirstTransportTime,
                          firsttimes.AtHospitalTime as FirstAtHospitalTime,
                          firsttimes.AvailableTime as FirstAvailableTime,
                          il.CoordinateX as LocationXCoord,
                          il.CoordinateY as LocationYCoord,
                          g.Grid as LocationGrid

                          from
                          cad.FireIncident fi
                          join cad.[Call]c
                          on c.IncidentID = fi.IncidentID
                          join cad.Assignment a
                          on a.FireIncidentID = fi.FireIncidentID
                          join cad.AssignmentFireLog afl
                          on a.AssignmentID = afl.AssignmentID
                          join cad.Incident i
                          on fi.IncidentID = i.IncidentID
                          join cad.Roster r
                          on r.RosterID = a.RosterID
                          join adm.FireUnit fu
                          on fu.UnitID = r.UnitID
                          join adm.Unit u
                          on u.UnitID= fu.UnitID
                          join adm.Nature n
                          on n.NatureID = fi.NatureID
                          join cad.IncidentLocation il
                          on il.IncidentLocationID = fi.IncidentLocationID
                          left join adm.Grid g
                          on g.GridID = il.GridID
                          left join adm.Area area
                          on area.AreaID = il.FireAreaID
                          left join adm.Zone zone
                          on zone.ZoneID = il.FireZoneID
                          left join adm.Jurisdiction j
                          on j.JurisdictionID = il.FireJurisdictionID
                          left join adm.CallCategory cc
                          on fi.CallCategoryID = cc.CallCategoryID
                          left join adm.UnitUnitType uut
                          on uut.UnitID = u.UnitID
                          and uut.SortOrder = 1
                          left join adm.UnitType ut
                          on a.ActingUnitTypeID = ut.UnitTypeID
                          left join adm.[User] ct
                          on c.CalltakerUserID = ct.UserID
                          left join adm.[User] dusr
                          on fi.DispatcherUserID = dusr.UserID
                          left join adm.FireStation fs
                          on fs.FireStationID = fu.FireStationID
                          left join adm.Agency agent
                          on u.AgencyID = agent.AgencyID
                          left join adm.Agency cagent
                          on il.FireAgencyID = cagent.AgencyID
                          outer apply (
                          SELECT
                          min(afl1.EnrouteTime) as EnrouteTime
                          ,min(afl1.OnSceneTime) as OnSceneTime
                          ,min(afl1.DispatchedTime) as DispatchedTime
                          ,min(afl1.StagedTime) as StagedTime
                          ,min(afl1.AvailableTime) as AvailableTime
                          ,min(afl1.AtHospitalTime) as AtHospitalTime
                          ,min(afl1.EnrouteToHospitalTime) as EnrouteToHospitalTime
                          FROM
                          cad.Assignment a1
                          JOIN cad.AssignmentFireLog afl1
                          ON a1.AssignmentID = afl1.AssignmentID
                          WHERE
                          a1.FireIncidentID = a.FireIncidentID
                          ) firsttimes
                          where
                          c.ReceivedDate between '",firstDate, "' and '", lastDate, "'
                          and agent.Identifier = '", agency, "'")
  CADData <- dbGetQuery(con, queryStatement)

  # Fix TZ
  CADData <- CADData %>% mutate_if(is.POSIXct, with_tz, tzone=tzone)

  #disconnect
  odbc::dbDisconnect(con)

  CADData
}

