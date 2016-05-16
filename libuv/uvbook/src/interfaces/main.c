#include <stdio.h>
#include <uv.h>

int main()
{
	char buf[512];
	uv_interface_address_t *info;
	int count, i;

	uv_interface_addresses(&info, &count);
	i = count;

	printf("Number of interfaces: %d\n", count);
	while (i--)
	{
		uv_interface_address_t iface = info[i];

		printf("Name: %s\n", iface.name);
		printf("Internal? %s\n", iface.is_internal ? "Yes" : "No");

		if (iface.address.address4.sin_family == AF_INET)
		{
			uv_ip4_name(&iface.address.address4, buf, sizeof(buf));
			printf("IPv4 address: %s\n", buf);
		}
		else if (iface.address.address4.sin_family == AF_INET6)
		{
			uv_ip6_name(&iface.address.address6, buf, sizeof(buf));
			printf("IPv6 address: %s\n", buf);
		}

		printf("\n");
	}

	uv_free_interface_addresses(info, count);
	return 0;
}
