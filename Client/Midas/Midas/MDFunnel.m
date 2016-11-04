//
//  MDFunnel.m
//  Midas
//
//  Created by Thomas Günzel on 09/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDFunnel.h"

#import "MDFilter.h"
#import "MDFilterGroup.h"

#import "MDImageGenerators.h"

@interface MDFunnel()<MDFilterUpdateDelegate>

@end

@implementation MDFunnel

- (instancetype)init
{
	self = [super init];
	if (self) {
		_filtered = [[NSMutableDictionary alloc] init];
		_rendered = [[NSMutableDictionary alloc] init];
		
		_filter = [[MDFilterGroup alloc] initRootFilterGroup];
		_filter.delegate = self;
		
		_visualizationType = MDVisualizationTypeHeatMap;
	}
	return self;
}

-(void)setInput:(NSArray *)input {
	if(_input == input) {
		return;
	}
	
	_input = input;
	
	[self renderImageForFilterNamed:nil];
	
	NSArray *keys = _filtered.allKeys;
	for (NSString *k in keys) {
		[self filterChanged:_filter forFilterName:k];
	}
}

-(void)filterChanged:(MDFilter *)filter forFilterName:(NSString *)filterName {
	NSLog(@"Changed: %@",filterName);
	
	NSArray *filtered = [self updateFilteredForFilterName:filterName];
	[_filtered setValue:filtered forKey:filterName];
	[self.delegate funnel:self changedFiltered:filtered forFilterName:filterName];
	[self renderImageForFilterNamed:filterName];
}

-(void)renderImageForFilterNamed:(NSString*)filterName {
	dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_BACKGROUND, 0), ^{
		Class generator = nil;
		if(self.visualizationType == MDVisualizationTypeHeatMap) {
			generator = [MDHeatMapImageGenerator class];
		} else if(self.visualizationType == MDVisualizationTypeCluster) {
			generator = [MDClusterImageGenerator class];
		} else {
			[_rendered setValue:nil forKey:(filterName ? filterName : @"")];
			return;
		}
		
		UIImage *rendered = [generator imageForTouchSequences:[self filteredForFilterName:filterName]];
		[_rendered setValue:rendered forKey:(filterName ? filterName : @"")];
		dispatch_async(dispatch_get_main_queue(), ^{
			[self.delegate funnel:self changedImage:rendered forFilterName:filterName];
		});
	});
}

-(void)filter:(NSString*)filterName {
	
}

-(NSArray *)filteredForFilterName:(NSString *)filterName {
	if(filterName == nil) {
		return _input;
	}
	NSArray *cached = [_filtered objectForKey:filterName];
	if(cached) {
		return cached;
	} else {
		return _input;
	}
}

-(UIImage *)imageForFilterName:(NSString *)filterName {
	if(filterName == nil) {
		return [_rendered valueForKey:@""];
	}
	UIImage *cached = [_rendered objectForKey:filterName];
	if(cached) {
		return cached;
	} else {
		return [_rendered valueForKey:@""];
	}
}

-(NSArray*)updateFilteredForFilterName:(NSString*)filterName {
	NSMutableArray *filtered = [[NSMutableArray alloc] init];
	for (id obj in _input) {
		if([_filter matchesRecording:obj forFilterNamed:filterName]) {
			[filtered addObject:obj];
		}
	}
	return filtered;
}

-(void)setVisualizationType:(MDVisualizationType)visualizationType {
	if(visualizationType == _visualizationType) {
		return;
	}
	
	_visualizationType = visualizationType;
	NSArray *names = _rendered.allKeys;
	[((NSMutableDictionary*)_rendered) removeAllObjects];
	for (NSString *name in names) {
		[self renderImageForFilterNamed:name];
	}
	
}

@end
