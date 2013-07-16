using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;

namespace WpfMemoryStatus
{
    class MemoryStatus
    {
        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
        struct MEMORYSTATUS
        {
            public Int32 dwLength;
            public Int32 dwMemoryLoad;
            public UInt32 dwTotalPhys;
            public UInt32 dwAvailPhys;
            public UInt32 dwTotalPageFile;
            public UInt32 dwAvailPageFile;
            public UInt32 dwTotalVirtual;
            public UInt32 dwAvailVirtual;
        };

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode)]
        extern static void GlobalMemoryStatus(ref MEMORYSTATUS status);

        static private string BytesToHumanReadableString(UInt32 value)
        {
            string result;
            if (value > (1024 * 1024 * 1024))
            {
                result = String.Format("{0} Gb", ((double)value) / (1024 * 1024 * 1024));
            }
            else if (value > (1024 * 1024))
            {
                result = String.Format("{0} Mb", ((double)value) / (1024 * 1024));
            }
            else if (value > 1024)
            {
                result = String.Format("{0} Kb", ((double)value) / 1024);
            }
            else
            {
                result = String.Format("{0} Bytes", value);
            }
            return result;
        }

        MEMORYSTATUS status = new MEMORYSTATUS();
        public MemoryStatus()
        {
            status.dwLength = Marshal.SizeOf(status);
            GlobalMemoryStatus(ref status);
        }

        public string TotalPhys
        {
            get { return BytesToHumanReadableString(this.status.dwTotalPhys); }
        }

        public string AvailPhys
        {
            get { return BytesToHumanReadableString(this.status.dwAvailPhys); }
        }

        public string TotalPageFile
        {
            get { return BytesToHumanReadableString(this.status.dwTotalPageFile); }
        }

        public string AvailPageFile
        {
            get { return BytesToHumanReadableString(this.status.dwAvailPageFile); }
        }

        public string TotalVirtual
        {
            get { return BytesToHumanReadableString(this.status.dwTotalVirtual); }
        }

        public string AvailVirtual
        {
            get { return BytesToHumanReadableString(this.status.dwAvailVirtual); }
        }
    }
}
