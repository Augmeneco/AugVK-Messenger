# AugVK
## _Мессенджер ВК_

AugVK -- это мессенджер для соц. сети ВКонтакте написанный на Lazarus'е с минимальными зависимостями и небольшим размером
Вдохновлён Telegram'ом и потому заимствует многие из его идей. 

## Возможности

- Имеет компактный режим интерфейса
- Мало зависимостей
- Мало весит / компактный
- Портируемый на все платформы на которые есть Lazarus (пока что в теории)
- Поддержка тачскринов (пока что в разработке)
- Поддержка мобильного Linux'а (pinephone, etc.)

## Сборка

### Требования
- Lazarus >= 2.0.10
- Free Pascal >= 3.2.0
- BGRAControls

Установите пакет `augcontrols.lpk` из папки `components/`.
Скомпилируйте с помощью `lazbuild` или Lazarus файл `AugVK.lpr` в режиме `Default` если вы хотите собрать под вашу систему
