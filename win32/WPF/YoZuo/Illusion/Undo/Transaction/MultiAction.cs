using System.Collections.Generic;

namespace Illusion
{
    /// <summary>
    ///  List class for <see cref="IAction"/>.
    /// </summary>
    public class MultiAction : List<IAction>, IMultiAction
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="MultiAction"/> class.
        /// </summary>
        public MultiAction()
        {
            IsDelayed = true;
        }

        /// <summary>
        /// Gets or sets a value indicating whether this instance is delayed.
        /// </summary>
        /// <value>
        /// 	<c>true</c> if this instance is delayed; otherwise, <c>false</c>.
        /// </value>
        public bool IsDelayed { get; set; }

        /// <summary>
        /// Apply changes encapsulated by this object.
        /// </summary>
        /// <remarks>
        /// ExecuteCount++
        /// </remarks>
        public void Execute()
        {
            if (!IsDelayed)
            {
                IsDelayed = true;
                return;
            }

			if (!CanExecute())
			{
				return;
			}

            foreach (var action in this)
            {
                action.Execute();
            }
        }

        /// <summary>
        /// Undo changes made by a previous Execute call.
        /// </summary>
        /// <remarks>
        /// ExecuteCount--
        /// </remarks>
        public void UnExecute()
        {
			if (!CanUnExecute())
			{
				return;
			}

            var reversed = new List<IAction>(this);
            reversed.Reverse();
            foreach (var action in reversed)
            {
                action.UnExecute();
            }
        }

        /// <summary>
        /// For most Actions, CanExecute is true when ExecuteCount = 0 (not yet executed)
        /// and false when ExecuteCount = 1 (already executed once)
        /// </summary>
        /// <returns>true if an encapsulated action can be applied</returns>
        public bool CanExecute()
        {
            foreach (var action in this)
            {
                if (!action.CanExecute())
                {
                    return false;
                }
            }
            return true;
        }

        /// <summary>
        /// </summary>
        /// <returns>
        /// true if an action was already executed and can be undone
        /// </returns>
        public bool CanUnExecute()
        {
			var reversed = new List<IAction>(this);
			reversed.Reverse();

			foreach (var action in reversed)
            {
                if (!action.CanUnExecute())
                {
                    return false;
                }
            }
            return true;
        }

        /// <summary>
        /// Tries to merge.
        /// </summary>
        /// <param name="FollowingAction">The following action.</param>
        /// <returns></returns>
        public bool TryToMerge(IAction FollowingAction)
        {
            return false;
        }

        /// <summary>
        /// Defines if the action can be merged with the previous one in the Undo buffer
        /// This is useful for long chains of consecutive operations of the same type,
        /// e.g. dragging something or typing some text
        /// </summary>
        /// <value></value>
        public bool AllowToMergeWithPrevious { get; set; }
    }
}
