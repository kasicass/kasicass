package com.netease;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Date;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class NoodlesServlet extends HttpServlet {
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		PrintWriter writer = response.getWriter();

		String vegetable = request.getParameter("vegetable");
		if (vegetable == null) {
			vegetable = "Tomcat";
		}

		writer.println("<html><body>");
		writer.println("<h1> Noodles with " + vegetable + "</h1>");
		writer.println("</body></html>");
	}
}

