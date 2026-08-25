module Page.About exposing (repositoryUrl, view)

{-| What the app is, what it does, and where to say something about it.

Shown as a sheet over whichever screen the runner was on, so closing it puts
them straight back. The only links in the whole app live here.

-}

import Html exposing (Html, a, div, h3, li, p, text, ul)
import Html.Attributes exposing (class, href, rel, target)
import View.Form as Form


repositoryUrl : String
repositoryUrl =
    "https://github.com/programever/easy-pacer"


view : msg -> Html msg
view close =
    div [ class "about" ]
        [ h3 [] [ text "Trạm Kế" ]
        , p [ class "note" ]
            [ text "Trợ lý chạy trail cho người mới. Miễn phí, không quảng cáo, không cần tài khoản. Chạy hoàn toàn trên điện thoại của bạn và không gửi dữ liệu đi đâu — dùng được trên núi khi không có sóng." ]
        , h3 [ class "about-h" ] [ text "App làm được gì" ]
        , ul []
            [ feature "Nạp file GPX của giải: đọc luôn cự ly, độ cao, mặt cắt và các trạm có sẵn trong file."
            , feature "Điền bảng giờ đóng trạm (COT) của BTC, sắp xếp trạm bằng nút ↑ ↓. App soát lại kế hoạch trước khi chạy: trạm đặt sai km, COT lộn thứ tự, mục tiêu mâu thuẫn với COT. Giải chạy qua đêm hay sang ngày thứ hai đều tính đúng giờ."
            , feature "Đặt giờ mục tiêu cho từng trạm. Qua trạm rồi, app cho biết bạn sớm hay trễ so với kế hoạch của chính mình."
            , feature "Khi chạy: trạm kế tiếp còn bao xa, leo bao nhiêu, xuống bao nhiêu, còn bao lâu tới giờ đóng trạm — và toàn bộ chặng."
            , feature "Trạng thái COT theo màu: xanh là dư giờ, vàng là phải giữ nhịp, đỏ là nguy cơ không kịp — tính trên quãng đường đã quy đổi dốc (mặc định 100 m leo = 1000 m đường bằng, chỉnh được)."
            , feature "Mặt cắt độ cao rê được bằng ngón tay, có con trỏ chạy theo trên sơ đồ đường chạy."
            , feature "Không chạy nền, không hao pin: app chỉ đọc GPS lúc bạn bấm. Muốn thấy số liệu mới, bấm Lấy GPS hoặc nhập số km đang chạy."
            , feature "Lấy GPS: xác định bạn đang ở km nào, kể cả đoạn đi-về trùng vệt. Nếu lạc, chỉ hướng và khoảng cách để quay lại vệt."
            , feature "Chỉ cập nhật km khi GPS đủ chính xác (sai số dưới 50 m). Tín hiệu yếu thì giữ số cũ và báo cho bạn biết."
            , feature "Tin nhắn cầu cứu soạn sẵn, có toạ độ và mốc km gần nhất — gửi được khi chỉ còn sóng điện thoại."
            , feature "Tắt app, tải lại trang, vẫn tiếp tục đúng chỗ đang chạy."
            , feature "Lưu kế hoạch ra file để dùng lại hoặc gửi cho bạn chạy cùng."
            ]
        , h3 [ class "about-h" ] [ text "Góp ý và đóng góp" ]
        , p [ class "note" ]
            [ text "Mã nguồn mở trên GitHub. Thấy lỗi, muốn thêm tính năng, hay chỉ muốn nói app có ích — đều rất quý. Nếu app giúp được bạn, cho một ngôi sao nhé ⭐" ]
        , div [ class "about-links" ]
            [ link repositoryUrl "Xem mã nguồn và cho sao"
            , link (repositoryUrl ++ "/issues/new") "Báo lỗi hoặc góp ý"
            ]
        , Form.button "Đóng" close
        ]


feature : String -> Html msg
feature content =
    li [] [ text content ]


link : String -> String -> Html msg
link url label =
    a [ class "btn mini about-link", href url, target "_blank", rel "noopener noreferrer" ]
        [ text label ]
