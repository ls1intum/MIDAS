//
//  MDTouchVisualizationLayer.m
//  Midas
//
//  Created by Thomas Günzel on 29/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDTouchVisualizationLayer.h"

#import "MDTouchSequence.h"
#import "MDTouch.h"

@import UIKit;

CGRect circleAtPoint(CGFloat x, CGFloat y, CGFloat r) {
	CGRect z;
	z.origin.x = x-r;
	z.origin.y = y-r;
	z.size.width = 2.0*r;
	z.size.height = z.size.width;
	return z;
}

@interface MDTouchVisualizationLayer()


@property(readwrite,nonatomic,strong) CAGradientLayer *gradientLayer;
@property(readwrite,nonatomic,strong) UIImage *img;

@end

@implementation MDTouchVisualizationLayer


-(UIImage*)createImage {
	UIScreen *main = [UIScreen mainScreen];
	UIGraphicsBeginImageContextWithOptions(main.bounds.size, NO, main.scale);
	
	CGContextRef ctx = UIGraphicsGetCurrentContext();
	

	CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
	CGGradientRef myGradient;
	size_t num_locations = 2;
	CGFloat locations[2] = { 0.0, 1.0 };
	CGFloat components[8] = {
		1.0, 0.0, 0.0, 0.5,  // Start color
		1.0, 0.0, 0.0, 0.0 }; // End color
 
	myGradient = CGGradientCreateWithColorComponents (colorSpace, components,
													  locations, num_locations);
	
	for (MDTouchSequence *ts in _points) {
		for (MDTouch *t in ts.touches) {
			CGContextDrawRadialGradient(ctx, myGradient, t.location, 0.0, t.location, 25.0, kCGGradientDrawsAfterEndLocation);
		}
	}
	
	
	UIImage *img = UIGraphicsGetImageFromCurrentImageContext();
	UIGraphicsEndImageContext();
	
	return img;
}

-(void)setPoints:(NSArray<MDTouchSequence *> *)points {
	if(_points == points) {
		return;
	}
	
	_points = points;
	
	_img = [self createImage];
	self.contents = (__bridge id)_img.CGImage;
}

@end
