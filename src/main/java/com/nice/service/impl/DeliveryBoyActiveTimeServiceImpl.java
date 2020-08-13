/**
 *
 */
package com.nice.service.impl;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Date;
import java.util.Optional;

import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import com.nice.config.UserAwareUserDetails;
import com.nice.dto.DeliveryBoyActiveTimeDto;
import com.nice.exception.NotFoundException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.DeliveryBoyActiveTimeActiveTimeMapper;
import com.nice.model.DeliveryBoy;
import com.nice.model.DeliveryBoyActiveTime;
import com.nice.model.DeliveryBoyCurrentStatus;
import com.nice.model.UserLogin;
import com.nice.repository.DeliveryBoyActiveTimeRepository;
import com.nice.repository.DeliveryBoyCurrentStatusRepository;
import com.nice.service.DeliveryBoyActiveTimeService;
import com.nice.service.DeliveryBoyService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 07-Aug-2020
 */
@Service(value = "deliveryBoyActiveTimeServiceImpl")
public class DeliveryBoyActiveTimeServiceImpl implements DeliveryBoyActiveTimeService {

	@Autowired
	private DeliveryBoyActiveTimeRepository deliveryBoyActiveTimeRepository;

	@Autowired
	private DeliveryBoyActiveTimeActiveTimeMapper deliveryBoyActiveTimeActiveTimeMapper;

	@Autowired
	private DeliveryBoyCurrentStatusRepository deliveryBoyCurrentStatusRepository;
	@Autowired
	private DeliveryBoyService deliveryBoyService;
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Override
	public void updateDeliveryActiveTime(final Long deliveryPersonId) throws NotFoundException {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		Optional<DeliveryBoyActiveTime> optDeliveryBoyActiveTime = deliveryBoyActiveTimeRepository.findAllByDeliveryBoyIdAndRecordDate(deliveryPersonId,
				new Date());
		DeliveryBoy deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(deliveryPersonId);
		Optional<DeliveryBoyCurrentStatus> optDeliveryBoyCurrentStatus = deliveryBoyCurrentStatusRepository.findByDeliveryBoy(deliveryBoy);
		if (!optDeliveryBoyCurrentStatus.isPresent()) {
			throw new NotFoundException(messageByLocaleService.getMessage("delivery.boy.current.status.not.found", null));
		}
		/**
		 * If the deliveryBoyActiveTime exists for today then
		 */
		Date lastActiveTime = optDeliveryBoyCurrentStatus.get().getLastActivateTime();
		Long lastActiveTimeInMillisec = LocalDateTime.now().toEpochSecond(ZoneOffset.UTC) * 1000 < lastActiveTime.getTime() ? lastActiveTime.getTime()
				: LocalDateTime.now().toEpochSecond(ZoneOffset.UTC) * 1000;

		if (!optDeliveryBoyActiveTime.isPresent()) {
			DeliveryBoyActiveTimeDto deliveryBoyActiveTimeDto = new DeliveryBoyActiveTimeDto();
			deliveryBoyActiveTimeDto.setActive(true);
			deliveryBoyActiveTimeDto.setActiveTimeMinutes((LocalDateTime.now().toEpochSecond(ZoneOffset.UTC) * 1000 - lastActiveTimeInMillisec) / 60000);
			deliveryBoyActiveTimeDto.setRecordDate(new Date());
			DeliveryBoyActiveTime deliveryBoyActiveTime = deliveryBoyActiveTimeActiveTimeMapper.toEntity(deliveryBoyActiveTimeDto, userLogin.getId());
			deliveryBoyActiveTime.setDeliveryBoy(deliveryBoy);
			deliveryBoyActiveTimeRepository.save(deliveryBoyActiveTime);
		} else {
			Long activeTimeInMinutes = (LocalDateTime.now().toEpochSecond(ZoneOffset.UTC) * 1000 - lastActiveTimeInMillisec) / 60000;
			DeliveryBoyActiveTime deliveryBoyActiveTime = optDeliveryBoyActiveTime.get();
			deliveryBoyActiveTime.setActiveTimeMinutes(deliveryBoyActiveTime.getActiveTimeMinutes() + activeTimeInMinutes);
			deliveryBoyActiveTimeRepository.save(deliveryBoyActiveTime);
		}
	}

	@Override
	public Long getDeliveryBoyActiveTimeDetailsForToday(final Long deliveryPersonId) throws NotFoundException {
		Optional<DeliveryBoyCurrentStatus> optDeliveryBoyCurrentStatus = deliveryBoyCurrentStatusRepository.findById(deliveryPersonId);
		if (!optDeliveryBoyCurrentStatus.isPresent()) {
			throw new NotFoundException(messageByLocaleService.getMessage("delivery.boy.current.status", null));
		}
		DeliveryBoyCurrentStatus deliveryBoyCurrentStatus = optDeliveryBoyCurrentStatus.get();
		Optional<DeliveryBoyActiveTime> deliveryBoyActiveTime = deliveryBoyActiveTimeRepository.findAllByDeliveryBoyIdAndRecordDate(deliveryPersonId,
				new Date());

		if (!deliveryBoyActiveTime.isPresent()) {
			/**
			 * If last active time is null means the delivery boy is currently inactive and the total active time will be already
			 * there in the DB. </br>
			 * If the last active time is not null means the delivery boy already has session going on and the current session time
			 * needs to be added to the total active time
			 *
			 */
			if (deliveryBoyCurrentStatus.getLastActivateTime() == null) {
				return 0L;
			} else {
				Long lastActiveTimeInMillisec = DateTime.now().toDateTime(DateTimeZone.UTC).getMillis() > deliveryBoyCurrentStatus.getLastActivateTime()
						.getTime() ? deliveryBoyCurrentStatus.getLastActivateTime().getTime() : DateTime.now().toDateTime(DateTimeZone.UTC).getMillis();
				return (DateTime.now().toDateTime(DateTimeZone.UTC).getMillis() - lastActiveTimeInMillisec) / 60000;
			}

		} else {
			if (deliveryBoyCurrentStatus.getLastActivateTime() == null) {
				return deliveryBoyActiveTime.get().getActiveTimeMinutes();
			} else {
				Long lastActiveTimeInMillisec = LocalDateTime.now().toEpochSecond(ZoneOffset.UTC) * 1000 < deliveryBoyCurrentStatus.getLastActivateTime()
						.getTime() ? deliveryBoyCurrentStatus.getLastActivateTime().getTime() : LocalDateTime.now().toEpochSecond(ZoneOffset.UTC) * 1000;
				return deliveryBoyActiveTime.get().getActiveTimeMinutes()
						+ (LocalDateTime.now().toEpochSecond(ZoneOffset.UTC) * 1000 - lastActiveTimeInMillisec) / 60000;
			}
		}

	}
}
