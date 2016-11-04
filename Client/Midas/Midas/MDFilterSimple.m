//
//  MDFilterSimple.m
//  Midas
//
//  Created by Thomas Günzel on 08/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDFilterSimple.h"

#import "MDTouchSequence.h"

@interface MDFilterSimple()

@property(readwrite) BOOL internalActive;
@property(readwrite,nonatomic,strong) NSMutableDictionary<NSString*, NSMutableSet*> *acceptedByFilter;

@end


@implementation MDFilterSimple

-(instancetype)initWithName:(NSString *)name image:(UIImage *)image forKey:(NSString *)keyName inContext:(NSString *)contextType availableValues:(NSArray *)availableValues {
	self = [super initWithName:name image:image];
	if (self) {
		_keyName = keyName;
		_contextType = contextType;
		_availableValues = availableValues;
		
		_acceptedByFilter = [[NSMutableDictionary alloc] init];
	}
	return self;
}

+(instancetype)filterWithName:(NSString *)name image:(UIImage *)image forKey:(NSString *)keyName inContext:(NSString *)contextType availableValues:(NSArray *)availableValues {
	return [[MDFilterSimple alloc] initWithName:name image:image forKey:keyName inContext:contextType availableValues:availableValues];
}



-(NSSet *)acceptedValuesForFilterNamed:(NSString *)filterName {
	return [_acceptedByFilter valueForKey:filterName];
}

-(void)addAcceptedValue:(id)value forFilterNamed:(NSString *)filterName{
	NSMutableSet *set = [_acceptedByFilter valueForKey:filterName];
	if(set == nil) {
		set = [[NSMutableSet alloc] init];
		[_acceptedByFilter setValue:set forKey:filterName];
	}
	[set addObject:value];
	
	[self.delegate filterChanged:self forFilterName:filterName];
}

-(void)removeAcceptedValue:(id)value forFilterNamed:(NSString *)filterName{
	NSMutableSet *set = [_acceptedByFilter valueForKey:filterName];
	if(set != nil) {
		[set removeObject:value];
	}
	
	[self.delegate filterChanged:self forFilterName:filterName];
}

-(BOOL)activeForFilterNamed:(NSString *)filterName {
	NSMutableSet *acc = [_acceptedByFilter valueForKey:filterName];
	return acc.count > 0;
}

-(BOOL)value:(id)value isAcceptedForFilterNamed:(NSString *)filterName {
	NSSet *acc = [self acceptedValuesForFilterNamed:filterName];
	return [acc containsObject:value];
}

-(BOOL)matchesRecording:(id)recording forFilterNamed:(NSString*)filterName {
	NSSet *acc = [self acceptedValuesForFilterNamed:filterName];
	if(acc.count == 0) {
		return YES;
	}
	
	MDTouchSequence *ts = (MDTouchSequence*)recording;
	NSDictionary *ctx = [ts context:self.contextType];
	//NSLog(@"%@: %@",self.contextType,ctx);
	if(ctx == nil) {
		return NO;
	}
	
	id val = [ctx valueForKeyPath:self.keyName];
	if(val == nil) {
		return NO;
	}
	
	if([acc containsObject:val]) {
		return YES;
	} else {
		return NO;
	}
}

-(void)resetForFilterNamed:(NSString *)filterName {
	NSMutableSet *acc = [self.acceptedByFilter valueForKey:filterName];
	[acc removeAllObjects];
	[self.delegate filterChanged:self forFilterName:filterName];
}

@end
