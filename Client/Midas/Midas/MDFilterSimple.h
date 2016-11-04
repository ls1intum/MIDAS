//
//  MDFilterSimple.h
//  Midas
//
//  Created by Thomas Günzel on 08/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDFilter.h"

@class MDFilterValue;

@interface MDFilterSimple : MDFilter

-(instancetype)initWithName:(NSString*)name image:(UIImage*)image forKey:(NSString*)keyName inContext:(NSString*)contextType availableValues:(NSArray*)availableValues;
+(instancetype)filterWithName:(NSString*)name image:(UIImage*)image forKey:(NSString*)keyName inContext:(NSString*)contextType availableValues:(NSArray*)availableValues;

-(void)addAcceptedValue:(id)value forFilterNamed:(NSString*)filterName;
-(void)removeAcceptedValue:(id)value forFilterNamed:(NSString*)filterName;
-(NSSet*)acceptedValuesForFilterNamed:(NSString*)filterName;
-(BOOL)value:(id)value isAcceptedForFilterNamed:(NSString*)filterName;

@property(readonly,nonatomic,strong) NSString *contextType;
@property(readonly,nonatomic,strong) NSString *keyName;
@property(readonly,nonatomic,strong) NSArray<MDFilterValue*> *availableValues;

@end
