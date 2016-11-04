//
//  MDTimeContext.m
//  Midas
//
//  Created by Thomas Günzel on 28/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDTimeContext.h"

#import "MDEncodeTypes.h"

@implementation MDTimeContext

- (instancetype)init {
	self = [super init];
	if (self) {
		_locale = [NSLocale currentLocale];
		
		NSTimeZone *currentTimeZone = [NSTimeZone localTimeZone];
		_timezoneOffset = [currentTimeZone secondsFromGMT];
		_timezone = [currentTimeZone name];
		_timestamp = [NSDate timeIntervalSinceReferenceDate];// + ((60.0 * 60.0) * 70.0);
	}
	return self;
}


-(void)encodeWithEncoder:(MDEncoder *)encoder {
	NSString *localeIdentifier = _locale.localeIdentifier;
	uint8_t locale_size = MIN(localeIdentifier.length,254);
	
	uint8_t timezone_size = MIN(_timezone.length,254);
	int32_t timezone_offset = (int32_t)_timezoneOffset;
	
	size_t data_size = (3 * sizeof(uint8_t)) + locale_size + timezone_size + sizeof(int32_t) + sizeof(double);
	uint8_t *data = malloc(data_size);
	
	size_t pos = 0;
	data[pos++] = kMDEncoded_Context_Time;
	
//	*((int32_t*)&data[pos]) = timezone_offset;
	memcpy(&data[pos], &timezone_offset, sizeof(int32_t));
	pos += sizeof(int32_t);
	
	double time = _timestamp;
	memcpy(&data[pos], &time, sizeof(double));
	pos += sizeof(double);
	
	data[pos++] = timezone_size;
	
	memcpy(&data[pos], _timezone.UTF8String, timezone_size);
	pos += timezone_size;
	
	data[pos++] = locale_size;
	
	memcpy(&data[pos], localeIdentifier.UTF8String, locale_size);
	
	[encoder writeData:data ofLength:data_size];
}

@end
