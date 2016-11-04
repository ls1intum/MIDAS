//
//  MDSeguePreview.m
//  Midas
//
//  Created by Thomas Günzel on 16/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDSeguePreview.h"

#import "MDScreenshotCache.h"

@interface MDSeguePreview()

@property(readwrite,nonatomic,strong) UILabel *nameLabel;
@property(readwrite,nonatomic,strong) UIImageView *imageView;

@end


@implementation MDSeguePreview


- (instancetype)initWithFrame:(CGRect)frame {
	self = [super initWithFrame:frame];
	if (self) {
		_nameLabel = [[UILabel alloc] initWithFrame:self.bounds];
		_nameLabel.backgroundColor = [UIColor colorWithRed:0.0 green:0.5 blue:0.5 alpha:1.0];
		_nameLabel.textColor = [UIColor whiteColor];
		_nameLabel.font = [UIFont boldSystemFontOfSize:12.0];
		_nameLabel.textAlignment = NSTextAlignmentCenter;
		_nameLabel.numberOfLines = 0;
		
		_imageView = [[UIImageView alloc] initWithFrame:self.bounds];
		[self addSubview:_imageView];
		_imageView.hidden = YES;
		_imageView.contentMode = UIViewContentModeScaleAspectFit;
		
		[self addSubview:_nameLabel];
		
		
	}
	return self;
}

/*
// Only override drawRect: if you perform custom drawing.
// An empty implementation adversely affects performance during animation.
- (void)drawRect:(CGRect)rect {
    // Drawing code
}
*/


-(void)setViewControllerName:(NSString *)viewControllerName {
	if(_viewControllerName == viewControllerName) {
		return;
	}
	
	_viewControllerName = viewControllerName;
	
	NSRegularExpression *regexp = [NSRegularExpression
								   regularExpressionWithPattern:@"([a-z])([A-Z])"
								   options:0
								   error:NULL];
	NSString *newString = [regexp
						   stringByReplacingMatchesInString:_viewControllerName
						   options:0
						   range:NSMakeRange(0, _viewControllerName.length)
						   withTemplate:@"$1 $2"];
	
	_nameLabel.text = newString;
	
	UIImage *img = [[MDScreenshotCache sharedCache] screenshot:viewControllerName];
	if(img != nil) {
		_nameLabel.backgroundColor = [UIColor clearColor];
//		_nameLabel.hidden = YES;
		_imageView.image = img;
		_imageView.frame = self.bounds;
		_imageView.hidden = NO;
	}
}


@end
