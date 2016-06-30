using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ButtonTest01
{
    class BindingData
    {
        public BindingData()
        {
            ColorName = "Red";
        }

        private string _name = "Red";

        public string ColorName
        {
            get { return _name; }
            set { _name = value; }
        }
    }
}
