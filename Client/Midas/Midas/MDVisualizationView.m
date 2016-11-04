//
//  MDVisualizationView.m
//  Midas
//
//  Created by Thomas Günzel on 01/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDVisualizationView.h"
#import "MDTouchVisualizationLayer.h"

@interface MDVisualizationView()

@property(readwrite,nonatomic,strong) UIActivityIndicatorView *indicatorView;
@property(readwrite,nonatomic,strong) CALayer *touchLayer;

@end

@implementation MDVisualizationView

- (instancetype)initWithFrame:(CGRect)frame {
	self = [super initWithFrame:frame];
	if (self) {
//		_touchLayer = [CALayer layer];
//		_touchLayer.zPosition = 100;
//		_touchLayer.opacity = 0.75;
		
		_indicatorView = [[UIActivityIndicatorView alloc] initWithActivityIndicatorStyle:UIActivityIndicatorViewStyleWhiteLarge];
		CGRect oldFrame = _indicatorView.frame;
		oldFrame.origin.x = (self.frame.size.width/2.0) - (oldFrame.size.width/2.0);
		oldFrame.origin.y = (self.frame.size.height/2.0) - (oldFrame.size.height/2.0);
		_indicatorView.frame = oldFrame;
		_indicatorView.autoresizingMask = (UIViewAutoresizingFlexibleTopMargin |
										   UIViewAutoresizingFlexibleLeftMargin |
										   UIViewAutoresizingFlexibleRightMargin |
										   UIViewAutoresizingFlexibleBottomMargin);
		[self addSubview:_indicatorView];
		
		
		[_indicatorView startAnimating];

		
		self.layer.borderColor = [UIColor darkGrayColor].CGColor;
		self.layer.borderWidth = 1.0;
		[self.layer addSublayer:_touchLayer];
	}
	return self;
}

-(void)layoutSubviews {
	[super layoutSubviews];
	_underlyingView.frame = self.bounds;
	_touchLayer.frame = self.bounds;
}

-(void)setUnderlyingView:(UIView *)underlyingView {
	if(_underlyingView == underlyingView) {
		return;
	}
	
	[_underlyingView removeFromSuperview];
	
	_underlyingView = underlyingView;
	
	if(_underlyingView != nil) {
		[self insertSubview:_underlyingView atIndex:0];
		_underlyingView.frame = self.bounds;
	}
}

-(void)setOverlayView:(UIView *)overlayView {
	if(_overlayView == overlayView) {
		return;
	}
	
	[_overlayView removeFromSuperview];
	
	_overlayView = overlayView;
//	_touchLayer.contents = (id)_overlayView.CGImage;
	
	if(_overlayView == nil) {
		_indicatorView.hidden = NO;
		[_indicatorView startAnimating];
	} else {
		[_indicatorView stopAnimating];
		_indicatorView.hidden = YES;
		_overlayView.frame = self.bounds;
		[self addSubview:_overlayView];
	}
}

/*
// Only override drawRect: if you perform custom drawing.
// An empty implementation adversely affects performance during animation.
- (void)drawRect:(CGRect)rect {
    // Drawing code
}
*/

@end
