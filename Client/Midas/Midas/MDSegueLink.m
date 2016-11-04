//
//  MDSegueLink.m
//  Midas
//
//  Created by Thomas Günzel on 17/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDSegueLink.h"

#import "MDSeguePreview.h"

@interface MDSegueLink()

@property(readwrite,nonatomic,weak) MDSeguePreview *from;
@property(readwrite,nonatomic,weak) MDSeguePreview *to;
@property(readwrite,nonatomic,strong) CAShapeLayer *linkLayer;
@property(readwrite,nonatomic,strong) UILabel *percentLabel;

@end

@implementation MDSegueLink

- (instancetype)initFrom:(MDSeguePreview *)from to:(MDSeguePreview *)to {
	self = [super init];
	if (self) {
		self.clipsToBounds = NO;
		_from = from;
		_to = to;
		[self setupLink];
	}
	return self;
}

-(void)setupLink {
	_linkLayer = [CAShapeLayer layer];
	_linkLayer.masksToBounds = NO;
	_linkLayer.strokeColor = [UIColor greenColor].CGColor;
	_linkLayer.lineWidth = 2.0;
	_linkLayer.fillColor = nil;
	
	CGRect f = _from.frame;
	CGRect t = _to.frame;
	
	CGPoint start;
	CGPoint end;
	CGPoint mid;
	CGPoint cp1;
	CGPoint cp2;
	
	start.x = CGRectGetMidX(f);
	start.y = CGRectGetMidY(f);
	end.x = CGRectGetMidX(t);
	end.y = CGRectGetMidY(t);
	
	mid.x = (start.x + end.x) / 2.0;
	mid.y = (start.y + end.y) / 2.0;
	
	cp1.x = start.x + 150.0;//(start.x + end.x) / 2.0;
	cp2.x = end.x - 150.0;//cp1.x;
	cp1.y = start.y;
	cp2.y = end.y;
	
	CGMutablePathRef path = CGPathCreateMutable();
	CGPathMoveToPoint(path, NULL, start.x, start.y);
//	CGPathAddLineToPoint(path, NULL, end.x, end.y);
	CGPathAddCurveToPoint(path, NULL, cp1.x, cp1.y, cp2.x, cp2.y, end.x, end.y);
	
	_linkLayer.path = path;
	
	[self.layer addSublayer:_linkLayer];
	
	
	CGRect pRect;
	pRect.origin = mid;
	pRect.size.width = 50.0;
	pRect.size.height = 14.0;
	pRect.origin.x -= (pRect.size.width / 2.0);
	pRect.origin.y -= (pRect.size.height / 2.0);
	_percentLabel = [[UILabel alloc] initWithFrame:pRect];
	_percentLabel.text = @"100.0%";
	_percentLabel.font = [UIFont systemFontOfSize:12.0];
	_percentLabel.textColor = [UIColor whiteColor];
	_percentLabel.backgroundColor = [[UIColor greenColor] colorWithAlphaComponent:0.5];
	_percentLabel.textAlignment = NSTextAlignmentCenter;
	
	[self addSubview:_percentLabel];
}

-(void)setPercentage:(CGFloat)percentage {
	if(_percentage == percentage) {
		return;
	}
	
	_percentage = percentage;
	
	_percentLabel.text = [NSString stringWithFormat:@"%.1f%%",_percentage*100.0];
}

/*
// Only override drawRect: if you perform custom drawing.
// An empty implementation adversely affects performance during animation.
- (void)drawRect:(CGRect)rect {
    // Drawing code
}
*/

@end
