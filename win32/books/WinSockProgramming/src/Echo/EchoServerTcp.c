#include <stdio.h>
#include <WinSock2.h>

#define ECHO_DEF_PORT   7
#define ECHO_BUF_SIZE   256

int main(int argc, char *argv[])
{
	WSADATA wsa_data;
	SOCKET echo_soc = 0;
	SOCKET accept_soc = 0;
	struct sockaddr_in serv_addr, client_addr;
	unsigned short port = ECHO_DEF_PORT;
	int result = 0;
	int addr_len = sizeof(struct sockaddr_in);
	char recv_buf[ECHO_BUF_SIZE];

	if (argc == 2)
	{
		port = atoi(argv[1]);
	}

	WSAStartup(MAKEWORD(2,0), &wsa_data);
	echo_soc = socket(AF_INET, SOCK_STREAM, 0);

	serv_addr.sin_family = AF_INET;
	serv_addr.sin_port   = port;
	serv_addr.sin_addr.s_addr = INADDR_ANY;

	result = bind(echo_soc, (struct sockaddr *)&serv_addr, sizeof(serv_addr));
	if (result == SOCKET_ERROR)
	{
		printf("[EchoServer] bind error: %d\n", WSAGetLastError());
		closesocket(echo_soc);
		WSACleanup();
		return -1;
	}

	listen(echo_soc, SOMAXCONN);

	printf("[EchoServer] running on port %d ...\n", port);
	while (1)
	{
		accept_soc = accept(echo_soc, (struct sockaddr *)&client_addr, &addr_len);
		if (accept_soc == INVALID_SOCKET)
		{
			printf("[EchoServer] accept error: %d\n", WSAGetLastError());
			break;
		}

		result = recv(accept_soc, recv_buf, ECHO_BUF_SIZE, 0);
		if (result > 0)
		{
			recv_buf[result] = 0;
			printf("[EchoServer] recv: \"%s\", from %s\n", recv_buf,
				inet_ntoa(client_addr.sin_addr));

			result = send(accept_soc, recv_buf, result, 0);
		}

		closesocket(accept_soc);
	}

	closesocket(echo_soc);
	WSACleanup();

	return 0;
}