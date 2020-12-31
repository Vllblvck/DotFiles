import sys
import socket
from threading import Thread


HOSTNAME = "159.205.2.228"
THREADS_NR = 10
START_PORT = 1
END_PORT = 65535
TIMEOUT = 1


def scan_port(host, port):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    socket.setdefaulttimeout(TIMEOUT)
    port_status = s.connect_ex((host, port))
    s.close()

    return port_status


def print_open_ports(ports):
    host = socket.gethostbyname(HOSTNAME)

    for port in ports:
        port_status = scan_port(host, port)

        if port_status == 0:
            print(f"Port: {port} is open!")


def div_port_lst(lst, n):
    for i in range(0, len(lst), n):
        yield lst[i:i + n]


def main():
    chunk_size = (END_PORT - START_PORT) // THREADS_NR
    chunks = div_port_lst(range(START_PORT, END_PORT), chunk_size)

    threads = []
    try:
        for ports in chunks:
            thread = Thread(target=print_open_ports,
                            args=(ports, ), daemon=True)
            threads.append(thread)
            thread.start()

        for thread in threads:
            thread.join()

    except KeyboardInterrupt:
        print("Keyboard interrupt!")
        sys.exit()
    except socket.gaierror:
        print("\n Hostname could not be resolved!")
        sys.exit()
    except socket.error:
        print("\n Server not responding!")
        sys.exit()


if __name__ == "__main__":
    main()
