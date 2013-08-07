using System;
using System.Collections.Generic;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.DiagonalizeTheButtons
{
	class DiagonalPanel : FrameworkElement
	{
		List<UIElement> children = new List<UIElement>();
		Size sizeChildrenTotal;
		public static readonly DependencyProperty BackgroundProperty;

		static DiagonalPanel()
		{
			BackgroundProperty = DependencyProperty.Register("Background", typeof(Brush), typeof(DiagonalPanel),
				new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.AffectsRender));
		}

		public Brush Background
		{
			set { SetValue(BackgroundProperty, value); }
			get { return (Brush)GetValue(BackgroundProperty); }
		}

		public void Add(UIElement el)
		{
			children.Add(el);
			AddVisualChild(el);
			AddLogicalChild(el);
			InvalidateMeasure();
		}

		public void Remove(UIElement el)
		{
			children.Remove(el);
			RemoveVisualChild(el);
			RemoveLogicalChild(el);
			InvalidateMeasure();
		}

		public int IndexOf(UIElement el)
		{
			return children.IndexOf(el);
		}

		protected override int VisualChildrenCount
		{
			get { return children.Count; }
		}

		protected override Visual GetVisualChild(int index)
		{
			if (index >= children.Count)
				throw new ArgumentOutOfRangeException("index");
			return children[index];
		}

		protected override Size MeasureOverride(Size sizeAvailable)
		{
			sizeChildrenTotal = new Size(0, 0);
			foreach (UIElement child in children)
			{
				child.Measure(new Size(Double.PositiveInfinity, Double.PositiveInfinity));
				sizeChildrenTotal.Width += child.DesiredSize.Width;
				sizeChildrenTotal.Height += child.DesiredSize.Height;
			}
			return sizeChildrenTotal;
		}

		protected override Size ArrangeOverride(Size sizeFinal)
		{
			Point ptChild = new Point(0, 0);
			foreach (UIElement child in children)
			{
				Size sizeChild = new Size(0, 0);
				sizeChild.Width  = child.DesiredSize.Width * (sizeFinal.Width / sizeChildrenTotal.Width);
				sizeChild.Height = child.DesiredSize.Height * (sizeFinal.Height / sizeChildrenTotal.Height);
				child.Arrange(new Rect(ptChild, sizeChild));
				ptChild.X += sizeChild.Width;
				ptChild.Y += sizeChild.Height;
			}
			return sizeFinal;
		}

		protected override void OnRender(DrawingContext dc)
		{
			dc.DrawRectangle(Background, null, new Rect(new Point(0,0), RenderSize));
		}
	}
}

