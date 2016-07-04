using System;

namespace Illusion
{
    /// <summary>
    ///  Defines the transaction contract.
    /// </summary>
    public interface ITransaction : IDisposable
    {
        /// <summary>
        /// Gets the accumulating action.
        /// </summary>
        /// <value>The accumulating action.</value>
        IMultiAction AccumulatingAction { get; }
        /// <summary>
        /// Gets or sets a value indicating whether this instance is delayed.
        /// </summary>
        /// <value>
        /// 	<c>true</c> if this instance is delayed; otherwise, <c>false</c>.
        /// </value>
        bool IsDelayed { get; set; }
    }
}
