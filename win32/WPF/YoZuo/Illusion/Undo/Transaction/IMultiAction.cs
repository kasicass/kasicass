using System.Collections.Generic;

namespace Illusion
{
    /// <summary>
    ///  Support multi actions.
    /// </summary>
    public interface IMultiAction : IAction, IList<IAction>
    {
        /// <summary>
        /// Gets or sets a value indicating whether this instance is delayed.
        /// </summary>
        /// <value>
        /// 	<c>true</c> if this instance is delayed; otherwise, <c>false</c>.
        /// </value>
        bool IsDelayed { get; set; }
    }
}
