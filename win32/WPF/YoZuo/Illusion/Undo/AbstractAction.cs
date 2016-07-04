namespace Illusion
{
    /// <summary>
    ///  Base class of action.
    /// </summary>
    public abstract class AbstractAction : IAction
    {
        /// <summary>
        /// Gets or sets the execute count.
        /// </summary>
        /// <value>The execute count.</value>
        protected int ExecuteCount { get; set; }

        /// <summary>
        /// Apply changes encapsulated by this object.
        /// </summary>
        /// <remarks>
        /// ExecuteCount++
        /// </remarks>
        public virtual void Execute()
        {
            if (!CanExecute())
            {
                return;
            }
            ExecuteCore();
            ExecuteCount++;
        }

        /// <summary>
        /// Override execute core to provide your logic that actually performs the action
        /// </summary>
        protected abstract void ExecuteCore();

        /// <summary>
        /// Undo changes made by a previous Execute call.
        /// </summary>
        /// <remarks>
        /// ExecuteCount--
        /// </remarks>
        public virtual void UnExecute()
        {
            if (!CanUnExecute())
            {
                return;
            }
            UnExecuteCore();
            ExecuteCount--;
        }

        /// <summary>
        /// Override this to provide the logic that undoes the action
        /// </summary>
        protected abstract void UnExecuteCore();

        /// <summary>
        /// For most Actions, CanExecute is true when ExecuteCount = 0 (not yet executed)
        /// and false when ExecuteCount = 1 (already executed once)
        /// </summary>
        /// <returns>
        /// true if an encapsulated action can be applied
        /// </returns>
        public virtual bool CanExecute()
        {
            return ExecuteCount == 0;
        }

        /// <summary>
        /// </summary>
        /// <returns>
        /// true if an action was already executed and can be undone
        /// </returns>
        public virtual bool CanUnExecute()
        {
            return !CanExecute();
        }

        /// <summary>
        /// If the last action can be joined with the followingAction,
        /// the following action isn't added to the Undo stack,
        /// but rather mixed together with the current one.
        /// </summary>
        /// <param name="followingAction"></param>
        /// <returns>
        /// true if the FollowingAction can be merged with the
        /// last action in the Undo stack
        /// </returns>
        public virtual bool TryToMerge(IAction followingAction)
        {
            return false;
        }

        private bool mAllowToMergeWithPrevious = true;
        /// <summary>
        /// Defines if the action can be merged with the previous one in the Undo buffer
        /// This is useful for long chains of consecutive operations of the same type,
        /// e.g. dragging something or typing some text
        /// </summary>
        public bool AllowToMergeWithPrevious
        {
            get
            {
                return mAllowToMergeWithPrevious;
            }
            set
            {
                mAllowToMergeWithPrevious = value;
            }
        }
    }
}
