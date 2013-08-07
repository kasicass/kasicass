using System;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace Petzold.PaintOnCanvasClone
{
	public class CanvasClone : Panel
	{
		public static readonly DependencyProperty LeftProperty;
		public static readonly DependencyProperty TopProperty;

		static CanvasClone()
		{
			LeftProperty = DependencyProperty.RegisterAttached("Left", typeof(double), typeof(CanvasClone),
				new FrameworkPropertyMetadata(0.0, FrameworkPropertyMetadataOptions.AffectsParentArrange));
			TopProperty = DependencyProperty.RegisterAttached("Top", typeof(double), typeof(CanvasClone),
				new FrameworkPropertyMetadata(0.0, FrameworkPropertyMetadataOptions.AffectsParentArrange));
		}

		public static void SetLeft(DependencyObject obj, double value)
		{
			obj.SetValue(LeftProperty, value);
		}

		public static double GetLeft(DependencyObject obj)
		{
			return (double)obj.GetValue(LeftProperty);
		}

		public static void SetTop(DependencyObject obj, double value)
		{
			obj.SetValue(TopProperty, value);
		}

		public static double GetTop(DependencyObject obj)
		{
			return (double)obj.GetValue(TopProperty);
		}

		protected override Size MeasureOverride(Size sizeAvailable)
		{
			foreach (UIElement child in InternalChildren)
			{
				child.Measure(new Size(Double.PositiveInfinity, Double.PositiveInfinity));
			}
			return base.MeasureOverride(sizeAvailable);
		}

		protected override Size ArrangeOverride(Size sizeFinal)
		{
			foreach (UIElement child in InternalChildren)
			{
				child.Arrange(new Rect(new Point(GetLeft(child), GetTop(child)), child.DesiredSize));
			}
			return sizeFinal;
		}
	}
}

