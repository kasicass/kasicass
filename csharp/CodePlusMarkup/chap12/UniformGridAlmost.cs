using System;
using System.Windows;
using System.Windows.Input;
using System.Windows.Controls;
using System.Windows.Media;

namespace Petzold.DuplicateUniformGrid
{
	class UniformGridAlmost : Panel
	{
		public static readonly DependencyProperty ColumnsProperty;

		static UniformGridAlmost()
		{
			ColumnsProperty = DependencyProperty.Register("Columns", typeof(int), typeof(UniformGridAlmost),
				new FrameworkPropertyMetadata(1, FrameworkPropertyMetadataOptions.AffectsMeasure));
		}

		public int Columns
		{
			set { SetValue(ColumnsProperty, value); }
			get { return (int)GetValue(ColumnsProperty); }
		}

		public int Rows
		{
			get { return (InternalChildren.Count + Columns - 1) / Columns; }
		}

		protected override Size MeasureOverride(Size sizeAvailable)
		{
			Size sizeChild = new Size(sizeAvailable.Width/Columns, sizeAvailable.Height/Rows);
			double maxwidth = 0;
			double maxheight = 0;
			foreach (UIElement child in InternalChildren)
			{
				child.Measure(sizeChild);
				maxwidth = Math.Max(maxwidth, child.DesiredSize.Width);
				maxheight = Math.Max(maxheight, child.DesiredSize.Height);
			}
			return new Size(Columns*maxwidth, Rows*maxheight);
		}

		protected override Size ArrangeOverride(Size sizeFinal)
		{
			Size sizeChild = new Size(sizeFinal.Width / Columns, sizeFinal.Height / Rows);
			for (int index = 0; index < InternalChildren.Count; index++)
			{
				int row = index / Columns;
				int col = index % Columns;

				Rect rectChild = new Rect(new Point(col*sizeChild.Width, row*sizeChild.Height), sizeChild);
				InternalChildren[index].Arrange(rectChild);
			}
			return sizeFinal;
		}
	}
}

