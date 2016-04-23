import java.sql.SQLException;
import java.sql.DriverManager;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;

public class HelloCursor {
    static final String JDBC_DRIVER = "com.mysql.jdbc.Driver";
    static final String DB_URL = "jdbc:mysql://localhost/hello?useSSL=false&useCursorFetch=true";
    static final String USER = "kasicass";
    static final String PASSWORD = "123456";
    
    public static void helloworld() throws ClassNotFoundException {
        Connection conn = null;
        PreparedStatement ptmt = null;
        ResultSet rs = null;
        
        Class.forName(JDBC_DRIVER);
        try {
            conn = DriverManager.getConnection(DB_URL, USER, PASSWORD);
            ptmt = conn.prepareStatement("SELECT * FROM userTable WHERE age < ?");
            ptmt.setFetchSize(2);
            ptmt.setInt(1, 30);
            rs = ptmt.executeQuery();
            while (rs.next()) {
                System.out.println("Hello " + rs.getString("userName"));
            }
        } catch (SQLException e) {
            e.printStackTrace();
        } finally {
            try {
                // rs.close() && stmt.close(), no needed. conn.close() already does that.
                
                //if (rs != null) rs.close();
                //if (stmt != null) stmt.close();
                if (conn != null) conn.close();
            } catch (SQLException e) {
                // ignore
            }
        }
    }
    
    public static void main(String[] args) throws ClassNotFoundException {
        helloworld();
    }
}