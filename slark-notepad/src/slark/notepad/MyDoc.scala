package slark.notepad

/**
 * @author a554114
 */
object MyDoc extends Document {
  
  def java(content: String) = Snippet("Java", content)
  
  override def title = "ReportWriter DB Connection Enhancement"
  
  override def content = List(
    "Current DB connection utilities" <-| ( 
        "DbAgentAn interface for all database operations.",
        "OraDbAgent: An implementation of DbAgent interface which holds a connection at its lifecycle.",
        "DbAgentFactory: A helper class for creating DbAgent instance.",
        "ReportDatabaseException: An exception for database operations."),
    "How is DbAgent instances used now" <-| (
        "Main thread holds one DbAgent instance, and use it for querying new report request, report schedule changes." ->|
            java("""
              |ReportScheduler(ReportSetting setting)
              |     throws ReportSettingException, ReportDatabaseException,
              |     SchedulerException {
              |     agent = DbAgentFactory.getDbAgent(DbAgentFactory.ORACLE, setting);
              |     scheduleManager = new ReportWriterScheduleManager(agent);
              |     logger.info("Report scheduler is inited.");
              |}"""),
        "Report schedule threads share DB connection with main thread, and use it for creating report request." ->| 
            java("""
              |DbAgent agent =
              |    (DbAgent)dataMap.get(ReportWriterScheduleManager.DB_AGENT);
              |ReportScheduleJob job =
              |    (ReportScheduleJob)dataMap.get(ReportWriterScheduleManager.JOB_DETAILS);
              |try {
              |    logger.info("create request for schedule :" + job.getJobId());
              |    agent.createRequestForJob(job);
              |}
              |catch (SQLException e) {
              |    logger.error(e);
              |    throw new JobExecutionException(e, false);
              |}"""),
        "Report threads maintains a DB connection pool, when one thread is start executing, a connection is lent from the pool, which is used to querying report setting, update request status, and return to the pool when the execution finished." ->| 
            java("""
              |DbAgent agent = dbAgentPool.get();
              |try {
              |    ReportExport export = ReportExportFactory.getReportExport(report.getReportType(), setting, agent);
              |    export.export(report);
              |} finally {
              |      dbAgentPool.returnTo(agent);
              |      LOG.info("End of processing the request " + report.getRequest().getRequestId());
              |}"""),
        "Japser threads share DB connection with report thread, and use it for querying report data." ->|
            java("""
              |print = JasperFillManager.fillReport(jasperReport, params,
              |        report.getRequest().getTimeZoneManager()
              |                .getTimeZoneSpecificConnection(agent));""")),
    "Problem" <-| (
        "Currently, all database connections will not be closed."),
    "Expectation" <-| (
        "DB connections should be release after a certain time span if unused."),
    "Solution" <-| (
        "Use a C3P0 connection pool with min pool size set to 0 and an appropriate connection idle time."),
    "Code changes" <-| (
        "Introduce a new interface ResourceStrategy to abstract difference strategy for creating and release a resource." ->|
            java("""
              |public abstract class ResourceStrategy<T> {
              |    public abstract T get() throws Exception;
              |    public abstract void release(T t) throws Exception;
              |    public abstract void close() throws Exception;
              |}"""),
        "Use DbAgentFactory as factory class for creating different DbAgent instances." ->|
            java("""
              |public abstract class DbAgentFactory {
              |    public abstract ResourceStrategy<Connection> getResourceStrategy();
              |    public final DbAgent getDbAgent();
              |    public static DbAgentFactory createFromSetting(ReportSetting setting) throws ReportSettingException;
              |}"""),
        "Modify DbAgent to adapter the above changes." <-| (
            "Add Methods" ->|
              java("""
                |public Connection getSystemConnection() throws SQLException;
                |public Connection getReportConnection(DtoReport report) throws SQLException;
                |public Connection getConnection() throws SQLException;
                |public void releaseConnection(Connection conn) throws SQLException;
                |public void close() throws SQLException;"""),
            "Remove method" ->|
              java("public void closeConnection();")),
        "Modify OraDbAgent to adapter the above changes." <-| (
            "Add methods" ->|
                java("""
                  |public Connection getSystemConnection() throws SQLException;
                  |public Connection getReportConnection(DtoReport report) throws SQLException;"""),
            "Replace currentConnection with getSystemConnection()"),
        "Introduce new environment variable: DRIVER_CLASS.",
        "Introduce new CM_CONFIGURATION records: USE_CONNECTION_POOL, MAX_POOL_SIZE, MAX_IDLE_TIME.",
        "Rewriter method JasperExport.runReport and JasperExport.loadAndFill to close connection when report is exported.",
        "Separate nested database operations."))
  
}