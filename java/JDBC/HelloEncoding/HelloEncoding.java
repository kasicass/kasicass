// javac -encoding utf-8 HelloEncoding.java
//
// mysql> show variables like '%character%';
//   character_set_server
//   character_set_database
//
// mysql> show create table TableName;
//   charset per table
//   charset per column
//
// 优先级：charset per column > charset per table > character_set_database > character_set_server
// JDBC 的 encoding 要设置得和 database 中的一致。 

import java.util.Set;
import java.util.HashSet;
import java.sql.SQLException;
import java.sql.DriverManager;
import java.sql.Connection;
import java.sql.Statement;

public class HelloEncoding {
    static final String JDBC_DRIVER = "com.mysql.jdbc.Driver";
    static final String DB_URL = "jdbc:mysql://localhost/hello?useSSL=false&characterEncoding=utf8";
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
			users.add("郭磊");
			users.add("李四");
			insertUsers(users);
    }
}
