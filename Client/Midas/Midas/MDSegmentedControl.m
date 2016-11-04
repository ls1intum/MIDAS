//
//  MDSegmentedControl.m
//  MidasVisualizeUI
//
//  Created by Thomas Günzel on 01/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDSegmentedControl.h"

@interface MDSegmentedControl() {
	CGFloat _itemWidth;
}

@property(readwrite,nonatomic,strong) UITapGestureRecognizer *tapGesture;

@end

@implementation MDSegmentedControl


-(void)setRootItems:(NSArray<MDSegmentedControlItem *> *)rootItems {
	if(_rootItems == rootItems) {
		return;
	}
	
	if(_tapGesture == nil) {
		[self setupTapGesture];
	}
	
	_rootItems = rootItems;
	
	for (MDSegmentedControlItem *itm in _rootItems) {
		[self addSubview:itm];
	}
	
	
	[self setActiveItemIndex:_activeItemIndex];
	
}

-(void)layoutSubviews {
	[super layoutSubviews];
	CGRect frame = self.bounds;
	frame.size.width = frame.size.width / _rootItems.count;
	_itemWidth = frame.size.width;
	
	for (MDSegmentedControlItem *itm in _rootItems) {
		itm.frame = frame;
		frame.origin.x += frame.size.width;
	}
}

-(void)setActiveItemIndex:(NSUInteger)activeItemIndex {
	[_rootItems enumerateObjectsUsingBlock:^(MDSegmentedControlItem *obj, NSUInteger idx, BOOL *stop) {
		obj.active = (idx == activeItemIndex);
	}];
}

-(void)setupTapGesture {
	_tapGesture = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(tap:)];
	[self addGestureRecognizer:_tapGesture];
}


-(void)tap:(UITapGestureRecognizer*)gesture {
	if(_rootItems.count == 0 || _itemWidth == 0.0) {
		// prevent division by 0
		return;
	}
	CGPoint p = [gesture locationInView:self];
	if(CGRectContainsPoint(self.bounds, p)) {
		NSUInteger idx = (NSUInteger)floor(p.x / _itemWidth);
		if(idx > _rootItems.count) { // can't be smaller than 0
			idx = _rootItems.count;
		}
		[self setActiveItemIndex:idx];
		[self.delegate segmentedControl:self didSelectItem:_rootItems[idx] atIndex:idx];
	} else {
		NSLog(@"[MDSegmentedControl] Point outside");
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
