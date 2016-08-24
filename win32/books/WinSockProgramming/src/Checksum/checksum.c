#include <stdio.h>
#include <string.h>

#define DATA_MAX_LEN   14

struct data_sum
{
	char data[DATA_MAX_LEN];
	unsigned short checksum;
};

unsigned short ip_checksum(unsigned short *buf, int buf_len)
{
	unsigned long checksum = 0;

	while (buf_len > 1)
	{
		checksum += *buf++;
		buf_len -= sizeof(unsigned short);
	}

	if (buf_len)
	{
		checksum += *(unsigned char*)buf;
	}

	checksum = (checksum >> 16) + (checksum & 0xffff);
	checksum += (checksum >> 16);

	return (unsigned short)(~checksum);
}

int main(int argc, char *argv[])
{
	struct data_sum msg = {"Hello World!", 0};
	int length = sizeof(msg);

	msg.checksum = ip_checksum((unsigned short *)&msg, length);
	printf("Calculate check sum: 0x%x\n", msg.checksum);

	msg.checksum = ip_checksum((unsigned short *)&msg, length);
	printf("Verify check sum: 0x%x\n", msg.checksum);

	return 0;
}