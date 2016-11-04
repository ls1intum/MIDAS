//
//  MDSegueView.m
//  Midas
//
//  Created by Thomas Günzel on 16/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDSegueView.h"

#import "MDSeguePreview.h"
#import "MDSegueLink.h"

#import "MDConnectionCount.h"

@interface MDSegueView()

@property(readwrite,nonatomic,strong) NSMutableArray *levels;
@property(readwrite,nonatomic,strong) NSMutableDictionary *viewMapping;
@property(readwrite,nonatomic,strong) NSMutableDictionary *linkMapping;


@end


@implementation MDSegueView

- (instancetype)initWithFrame:(CGRect)frame {
	self = [super initWithFrame:frame];
	if (self) {
		//self.backgroundColor = [UIColor lightGrayColor];
	}
	return self;
}


-(void)setConnections:(NSMutableDictionary *)connections {
	if(_connections == connections) {
		return;
	}
	
	_connections = connections;
	
	[self updateView];
}

-(void)setRootViewIdentifier:(NSString *)rootViewIdentifier {
	if(_rootViewIdentifier == rootViewIdentifier) {
		return;
	}
	
	_rootViewIdentifier = rootViewIdentifier;
	
	[self updateView];
}

-(void)updateView {
	if(_connections == nil || _rootViewIdentifier == nil) {
		return;
	}
	// first, split the connections into levels
	NSUInteger maxViews = [self generateLevels];
	// generate preview views
	[self createViewMapping:maxViews];
	// add links
	[self addLinks];
}

-(NSUInteger)generateLevels {
	_levels = [[NSMutableArray alloc] init];
	// level 0 = root view identifier
	[_levels addObject:[NSSet setWithObject:_rootViewIdentifier]];
	
	NSMutableSet *visited = [[NSMutableSet alloc] init];
	
	NSUInteger max = 1;
	
	NSMutableSet *queue = [[NSMutableSet alloc] initWithObjects:_rootViewIdentifier, nil];
	while([queue count] > 0) {
		NSMutableSet *current = [[NSMutableSet alloc] init];
		[visited unionSet:queue];
		for (NSString *vId in queue) {
			[current unionSet:[_connections valueForKey:vId]];
		}
		
		[current minusSet:visited];
		
		if([current count] > 0) {
			[_levels addObject:current];
			max = MAX(max, current.count);
		}
		
		queue = current;
	}
	
	return max;
}

-(void)createViewMapping:(NSUInteger)maxViews {
	_viewMapping = [[NSMutableDictionary alloc] init];
	CGSize viewSize = [[UIScreen mainScreen] bounds].size;
	viewSize.width /= 4.0;
	viewSize.height /= 4.0;
	
	CGSize ownSize;
	ownSize.height = maxViews * (viewSize.height * 1.5);
	ownSize.width = _levels.count * (viewSize.width * 2.0);
	
	[_levels enumerateObjectsUsingBlock:^(NSSet *views, NSUInteger idx, BOOL *stop) {
		CGFloat x = (viewSize.width / 2.0) + idx * (viewSize.width * 2.0);
		NSUInteger i = 0;
		CGFloat startY = (ownSize.height / 2.0) + (viewSize.height / 2.0);
		CGFloat endHeight = views.count * (viewSize.height * 1.5);
		startY -= (endHeight / 2.0);
		for (NSString *viewId in views) {
			CGRect viewFrame;
			viewFrame.size = viewSize;
			viewFrame.origin.x = x;
			viewFrame.origin.y = startY + (i * viewSize.height * 1.5);
			i++;
			MDSeguePreview *preview = [[MDSeguePreview alloc] initWithFrame:viewFrame];
			preview.viewControllerName = viewId;
			[self addSubview:preview];
			[_viewMapping setValue:preview forKey:viewId];
		}
	}];
}

-(void)addLinks {
	_linkMapping = [[NSMutableDictionary alloc] init];
	
	[_connections enumerateKeysAndObjectsUsingBlock:^(NSString *from, NSSet *conns, BOOL *stop) {
		MDSeguePreview *f = [_viewMapping valueForKey:from];
		for (NSString *to in conns) {
			MDSeguePreview *t = [_viewMapping valueForKey:to];
			MDSegueLink *link = [[MDSegueLink alloc] initFrom:f to:t];
			[self insertSubview:link atIndex:0];
			[_linkMapping setValue:link forKey:[NSString stringWithFormat:@"%@ %@",from,to]];
		}
	}];
}

-(void)setWeightedConnections:(NSDictionary *)weightedConnections {
	if(_weightedConnections == weightedConnections) {
		return;
	}
	
	_weightedConnections = weightedConnections;
	
	for (MDSegueLink *l in _linkMapping.objectEnumerator) {
		l.percentage = 0.0;
	}
	
	if(_weightedConnections == nil) {
		return;
	}
	
	
	
	[_weightedConnections enumerateKeysAndObjectsUsingBlock:^(NSString *key, MDConnectionCount *obj, BOOL * _Nonnull stop) {
		MDSegueLink *l = [_linkMapping valueForKey:key];
		if(l) {
			if(obj.all > 0) {
				l.percentage = (CGFloat)obj.count / (CGFloat)obj.all;
			} else {
				l.percentage = 0.0;
			}
		}
	}];
}

/*
// Only override drawRect: if you perform custom drawing.
// An empty implementation adversely affects performance during animation.
- (void)drawRect:(CGRect)rect {
    // Drawing code
}
*/

@end
