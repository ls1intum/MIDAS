//
//  MDDownloadedTouches.h
//  Midas
//
//  Created by Thomas Günzel on 29/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDDownloadedRecordings.h"

@class MDTouchSequence;

@interface MDDownloadedTouches : MDDownloadedRecordings

-(instancetype)initForView:(NSString*)viewIdentifier;

@property(readwrite,nonatomic) NSArray<MDTouchSequence*> *allRecordings;

@end
