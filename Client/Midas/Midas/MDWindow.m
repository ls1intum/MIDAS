//
//  MDWindow.m
//  Midas
//
//  Created by Thomas Günzel on 16/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDWindow.h"

#import "MDTracker.h"
#import "MDVisualizationViewController.h"

#import "UIViewController+MidasInternal.h"
#import "UIControl+MidasInternal.h"

@interface MDWindow() {
	CGAffineTransform _currentTransform;
}

@property(readwrite,nonatomic,strong) MDVisualizationViewController *visualizationViewController;
@property(readwrite,nonatomic,strong) UIViewController *originalViewController;

@end


@implementation MDWindow

-(instancetype)initWithFrame:(CGRect)frame {
	self = [super initWithFrame:frame];
	if (self) {
		[UIViewController md_swizzle];
		[UIControl md_swizzleSend];
		
	}
	return self;
}

//-(void)setRootViewController:(UIViewController *)rootViewController {
//	self.visualizationViewController.targetViewController = rootViewController;
//}

//-(UIViewController *)rootViewController {
//	if(self.visualizingMode == YES) {
//		return self.visualizationViewController;
//	}
//	return [super rootViewController];
//}

-(void)sendEvent:(UIEvent *)event {
	if(event.type == UIEventTypeMotion && event.subtype == UIEventSubtypeMotionShake && self.visualizingMode == NO) {
		self.visualizingMode = YES;
		return;
	}
	
	if(self.visualizingMode == NO) {
		[self.tracker logEvent:event fromWindow:self];
	} else if(event.type != UIEventTypeMotion) {
		//self.visualizingMode = NO;
	}
	[super sendEvent:event];
}

-(void)layoutSubviews {
	[super layoutSubviews];
}

-(void)setVisualizingMode:(BOOL)visualizingMode {
	if(_visualizingMode == visualizingMode) {
		return;
	}
	
	_visualizingMode = visualizingMode;
	
	NSLog(@"Visualization Mode: %i",_visualizingMode);
	
	
	if(_visualizingMode == YES) {
		if(self.visualizationViewController == nil) {
			self.visualizationViewController = [[MDVisualizationViewController alloc] init];
		}
		self.tracker.enabled = NO;
		UIViewController *oldRoot = self.rootViewController;
		_originalViewController = oldRoot;
		self.visualizationViewController.targetViewController = _originalViewController.topmostViewController;
		self.rootViewController = self.visualizationViewController;
		oldRoot.view.userInteractionEnabled = NO;
	} else {
		self.tracker.enabled = YES;
		//UIViewController *target = self.visualizationViewController.targetViewController;
		self.visualizationViewController.targetViewController = nil;
		self.visualizationViewController = nil;
		
		self.rootViewController = _originalViewController;
		_originalViewController.view.frame = self.bounds;
		_originalViewController.view.userInteractionEnabled = YES;
	}
	
//	self.rootViewController = [self rootViewController];
	/*if(_visualizingMode == YES) {
		self.rootViewController = [self rootViewController];
	} else {
		NSLog(@"Exiting Visualizing Mode");
		_currentTransform = CGAffineTransformIdentity;
		[self setNeedsLayout];
	}*/
}

@end
