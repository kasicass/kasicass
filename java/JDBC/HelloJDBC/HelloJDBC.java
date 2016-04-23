// <1> Install MySQL
// <2> Install MySQL Connector/J .jar (http://dev.mysql.com/downloads/connector/j/), Set it to CLASSPATH
//
// *Create Test DB*
// CREATE DATABASE hello;
//
// CREATE TABLE userTable
// (
// userName varchar(255),
// age int
// );
// 
// INSERT INTO hello VALUES ('Kasi', 30);
// INSERT INTO hello VALUES ('Phay', 28);

import java.sql.SQLException;
import java.sql.DriverManager;
import java.sql.Connection;
import java.sql.Statement;
import java.sql.ResultSet;

public class HelloJDBC {
    static final String JDBC_DRIVER = "com.mysql.jdbc.Driver";
    static final String DB_URL = "jdbc:mysql://localhost/hello?useSSL=false";
    static final String USER = "kasicass";
    static final String PASSWORD = "123456";
    
    public static void helloworld() throws ClassNotFoundException {
        Connection conn = null;
        Statement stmt = null;
        ResultSet rs = null;
        
        Class.forName(JDBC_DRIVER);
        try {
            conn = DriverManager.getConnection(DB_URL, USER, PASSWORD);
            stmt = conn.createStatement();
            rs = stmt.executeQuery("SELECT userName FROM userTable");
            while (rs.next()) {
                System.out.println("Hello " + rs.getString("userName"));
            }
        } catch (SQLException e) {
            e.printStackTrace();
        } finally {
            try {
                if (conn != null) conn.close();
                if (stmt != null) stmt.close();
                if (rs != null) rs.close();
            } catch (SQLException e) {
                // ignore
            }
        }
    }
    
    public static void main(String[] args) throws ClassNotFoundException {
        helloworld();
    }
}