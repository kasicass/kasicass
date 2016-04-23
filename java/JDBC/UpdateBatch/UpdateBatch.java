import java.util.Set;
import java.util.HashSet;
import java.sql.SQLException;
import java.sql.DriverManager;
import java.sql.Connection;
import java.sql.Statement;

public class UpdateBatch {
    static final String JDBC_DRIVER = "com.mysql.jdbc.Driver";
    static final String DB_URL = "jdbc:mysql://localhost/hello?useSSL=false";
    static final String USER = "kasicass";
    static final String PASSWORD = "123456";
    
    public static void insertUsers(Set<String> users) throws ClassNotFoundException {
        Connection conn = null;
        Statement stmt = null;
        
        Class.forName(JDBC_DRIVER);
        try {
            conn = DriverManager.getConnection(DB_URL, USER, PASSWORD);
            stmt = conn.createStatement();
						for (String user : users) {
							stmt.addBatch("INSERT INTO userTable (userName) VALUES ('" + user + "');");
						}
						stmt.executeBatch();
						stmt.clearBatch();
        } catch (SQLException e) {
            e.printStackTrace();
        } finally {
            try {
                if (conn != null) conn.close();
            } catch (SQLException e) {
                // ignore
            }
        }
    }
    
    public static void main(String[] args) throws ClassNotFoundException {
			Set<String> users = new HashSet<String>();
			users.add("GuoYi");
			users.add("ZhangSi");
			users.add("LiSan");
			insertUsers(users);
    }
}
