package mycode.plainoio;

import java.io.OutputStream;
import java.io.IOException;
import java.nio.charset.Charset;
import java.net.ServerSocket;
import java.net.Socket;

public class PlainOioServer {
	public void serve(int port) throws IOException {
		final ServerSocket socket = new ServerSocket(port);
		try {
			for (;;) {
				final Socket clientSocket = socket.accept();
				System.out.println("Accepted connection from " + clientSocket);
				new Thread(new Runnable() {
					@Override
					public void run() {
						OutputStream out;
						try {
							out = clientSocket.getOutputStream();
							out.write("Hi!\r\n".getBytes(Charset.forName("UTF-8")));
							out.flush();
							clientSocket.close();
						}
						catch (IOException e) {
							e.printStackTrace();
						}

						try {
							clientSocket.close();
						}
						catch (IOException ex) {
							// ignore on close
						}
					}
				}).start();
			}
		}
		catch (IOException e) {
			e.printStackTrace();
		}
	}

	public static void main(String[] args) throws Exception {
		new PlainOioServer().serve(8080);
	}
}
