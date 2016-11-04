//
//  MDDownloadedActions.h
//  Midas
//
//  Created by Thomas Günzel on 15/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDDownloadedRecordings.h"

@class MDRecordedAction;

@interface MDDownloadedActions : MDDownloadedRecordings

-(instancetype)initForView:(NSString*)viewIdentifier;

@property(readwrite,nonatomic) NSArray<MDRecordedAction*> *allRecordings;
@end
