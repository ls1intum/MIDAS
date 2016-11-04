//
//  MDImageGenerator.m
//  Midas
//
//  Created by Thomas Günzel on 10/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDHeatMapImageGenerator.h"

#import "MDTouchSequence.h"
#import "MDTouch.h"

@implementation MDHeatMapImageGenerator

+(UIImage *)imageForTouchSequences:(NSArray<MDTouchSequence *> *)touchSequences {
	UIScreen *main = [UIScreen mainScreen];
	UIGraphicsBeginImageContextWithOptions(main.bounds.size, NO, main.scale);
	
	CGContextRef ctx = UIGraphicsGetCurrentContext();
	
	
	CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
	CGGradientRef myGradient;
	size_t num_locations = 2;
	CGFloat locations[2] = { 0.0, 1.0 };
	CGFloat components[8] = {
		0.0, 1.0, 1.0, 0.5,  // Start color
		0.0, 1.0, 1.0, 0.0 }; // End color
 
	myGradient = CGGradientCreateWithColorComponents (colorSpace, components,
													  locations, num_locations);
	
	for (MDTouchSequence *ts in touchSequences) {
		for (MDTouch *t in ts.touches) {
			CGContextDrawRadialGradient(ctx, myGradient, t.location, 0.0, t.location, 25.0, kCGGradientDrawsAfterEndLocation);
		}
	}
	
	
	UIImage *img = UIGraphicsGetImageFromCurrentImageContext();
	UIGraphicsEndImageContext();
	
	return img;
}


@end
