//
//  MDClusterImageGenerator.m
//  Midas
//
//  Created by Thomas Günzel on 15/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDClusterImageGenerator.h"

#import "MDTouchSequence.h"
#import "MDTouch.h"

CGRect _CGRectForCircle(CGPoint center, CGFloat radius) {
	CGRect r;
	r.origin.x = center.x - radius;
	r.origin.y = center.y - radius;
	r.size.width = radius * 2.0;
	r.size.height = r.size.width;
	
	return r;
}

@import FBAnnotationClustering;

@implementation MDClusterImageGenerator

+(UIImage *)imageForTouchSequences:(NSArray<MDTouchSequence *> *)touchSequences {
	UIScreen *main = [UIScreen mainScreen];
	UIGraphicsBeginImageContextWithOptions(main.bounds.size, NO, main.scale);
	
	CGContextRef ctx = UIGraphicsGetCurrentContext();
	
	NSMutableArray *annotations = [[NSMutableArray alloc] init];
	
	for (MDTouchSequence *ts in touchSequences) {
		for (MDTouch *t in ts.touches) {
			[annotations addObject:t];
		}
	}
	
	FBClusteringManager *manager = [[FBClusteringManager alloc] initWithAnnotations:annotations];
	
	NSArray *clustered = [manager clusteredAnnotationsWithinCGRect:main.bounds withZoomScale:1.0];
	NSLog(@"Clustered: %@",clustered);
	NSUInteger max = 1;
	for (id<FBAnnotation> annotation in clustered) {
		if ([annotation isKindOfClass:[FBAnnotationCluster class]]) {
			FBAnnotationCluster *c = (FBAnnotationCluster*)annotation;
			max = MAX(c.annotations.count, max);
		}
	}
	
	CGContextSetRGBFillColor(ctx, 1.0, 0.0, 0.0, 0.75);
	
	for (id<FBAnnotation> annotation in clustered) {
		CGFloat radius = 100.0/(CGFloat)max;
		if([annotation isKindOfClass:[FBAnnotationCluster class]]) {
			radius = [[((FBAnnotationCluster*)annotation) annotations] count] * radius;
			CGContextFillEllipseInRect(ctx, _CGRectForCircle(annotation.fbPoint.point, radius));
		}
	}
	
	
	UIImage *img = UIGraphicsGetImageFromCurrentImageContext();
	UIGraphicsEndImageContext();
	
	return img;
}

@end
