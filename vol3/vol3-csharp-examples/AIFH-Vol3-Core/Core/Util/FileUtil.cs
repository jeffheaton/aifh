using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.Util
{
    public class FileUtil
    {
        public static void DownloadFile(string url, string target)
        {
            using (var client = new WebClient())
            {
                client.DownloadFile(url, target);
            }
        }
    }
}
