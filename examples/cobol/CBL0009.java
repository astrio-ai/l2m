import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;

public final class CBL0009 {
    private static final DecimalFormat CURRENCY_FORMAT = createCurrencyFormat();

    private CBL0009() {
    }

    public static void main(String[] args) throws IOException {
        Path inputPath = args.length > 0 ? Paths.get(args[0]) : Paths.get("ACCTREC.txt");
        Path outputPath = args.length > 1 ? Paths.get(args[1]) : Paths.get("PRTLINE.txt");

        List<AccountRecord> records;
        if (Files.exists(inputPath)) {
            records = readRecords(inputPath);
        } else {
            System.err.println("Input file not found (" + inputPath + "). Using sample data.");
            records = sampleRecords();
        }

        writeReport(outputPath, records);
    }

    private static List<AccountRecord> readRecords(Path inputPath) throws IOException {
        List<AccountRecord> records = new ArrayList<>();
        try (BufferedReader reader = Files.newBufferedReader(inputPath, StandardCharsets.UTF_8)) {
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty() || line.startsWith("#")) {
                    continue;
                }
                String[] rawParts = splitRecord(line);
                records.add(AccountRecord.from(rawParts));
            }
        }
        return records;
    }

    private static String[] splitRecord(String line) {
        String delimiter = line.contains("|") ? "\\|" : ",";
        String[] parts = line.split(delimiter, -1);
        for (int i = 0; i < parts.length; i++) {
            parts[i] = parts[i].trim();
        }
        return parts;
    }

    private static void writeReport(Path outputPath, List<AccountRecord> records) throws IOException {
        Path parent = outputPath.toAbsolutePath().getParent();
        if (parent != null) {
            Files.createDirectories(parent);
        }
        BigDecimal totalLimit = BigDecimal.ZERO;
        BigDecimal totalBalance = BigDecimal.ZERO;

        try (BufferedWriter writer = Files.newBufferedWriter(outputPath, StandardCharsets.UTF_8)) {
            writeHeaders(writer);
            for (AccountRecord record : records) {
                totalLimit = totalLimit.add(record.limit);
                totalBalance = totalBalance.add(record.balance);
                writeRecord(writer, record);
            }
            writeTotals(writer, totalLimit, totalBalance);
        }
    }

    private static void writeHeaders(BufferedWriter writer) throws IOException {
        LocalDate today = LocalDate.now();
        writer.write("Financial Report for");
        writer.newLine();
        writer.write(String.format("Year %04d  Month %02d  Day %02d", today.getYear(), today.getMonthValue(), today.getDayOfMonth()));
        writer.newLine();
        writer.newLine();
        writer.write("Account   Last Name           Limit         Balance");
        writer.newLine();
        writer.write("--------  --------------------  ------------  -------------");
        writer.newLine();
    }

    private static void writeRecord(BufferedWriter writer, AccountRecord record) throws IOException {
        String limitText = padCurrency(record.limit);
        String balanceText = padCurrency(record.balance);
        writer.write(String.format("%-8s  %-20s  %s  %s", record.accountNumber, record.lastName, limitText, balanceText));
        writer.newLine();
    }

    private static void writeTotals(BufferedWriter writer, BigDecimal totalLimit, BigDecimal totalBalance) throws IOException {
        writer.newLine();
        writer.write("                              --------------  --------------");
        writer.newLine();
        writer.write(String.format("                     Totals =  %s  %s", padCurrency(totalLimit), padCurrency(totalBalance)));
        writer.newLine();
    }

    private static String padCurrency(BigDecimal amount) {
        return String.format("%12s", formatCurrency(amount));
    }

    private static String formatCurrency(BigDecimal amount) {
        synchronized (CURRENCY_FORMAT) {
            return CURRENCY_FORMAT.format(amount);
        }
    }

    private static DecimalFormat createCurrencyFormat() {
        DecimalFormatSymbols symbols = new DecimalFormatSymbols(Locale.US);
        DecimalFormat format = new DecimalFormat("$#,##0.00;-$#,##0.00", symbols);
        format.setRoundingMode(RoundingMode.HALF_UP);
        return format;
    }

    private static BigDecimal parseMoney(String raw) {
        if (raw == null || raw.isEmpty()) {
            return BigDecimal.ZERO.setScale(2);
        }
        String cleaned = raw.replace("$", "").replace(",", "").trim();
        boolean negative = false;
        if (cleaned.startsWith("(") && cleaned.endsWith(")")) {
            negative = true;
            cleaned = cleaned.substring(1, cleaned.length() - 1);
        }
        if (cleaned.isEmpty()) {
            return BigDecimal.ZERO.setScale(2);
        }
        BigDecimal value = new BigDecimal(cleaned);
        value = value.setScale(2, RoundingMode.HALF_UP);
        return negative ? value.negate() : value;
    }

    private static List<AccountRecord> sampleRecords() {
        return Arrays.asList(
            new AccountRecord("00001234", new BigDecimal("2500.00"), new BigDecimal("1250.25"), "Jones", "Mary", "12 Pine Street", "Springfield", "IL", "", "Preferred customer"),
            new AccountRecord("00004567", new BigDecimal("5000.00"), new BigDecimal("4988.10"), "Chen", "Wei", "89 River Road", "Dayton", "OH", "", "Card holder"),
            new AccountRecord("00007890", new BigDecimal("1200.00"), new BigDecimal("-85.30"), "Garcia", "Luis", "45 Elm Ave", "Austin", "TX", "", "Overdraft monitoring")
        );
    }

    private static final class AccountRecord {
        private final String accountNumber;
        private final BigDecimal limit;
        private final BigDecimal balance;
        private final String lastName;
        private final String firstName;
        private final String streetAddress;
        private final String cityCounty;
        private final String usaState;
        private final String reserved;
        private final String comments;

        private AccountRecord(String accountNumber, BigDecimal limit, BigDecimal balance, String lastName,
                              String firstName, String streetAddress, String cityCounty, String usaState,
                              String reserved, String comments) {
            this.accountNumber = accountNumber;
            this.limit = limit.setScale(2, RoundingMode.HALF_UP);
            this.balance = balance.setScale(2, RoundingMode.HALF_UP);
            this.lastName = lastName;
            this.firstName = firstName;
            this.streetAddress = streetAddress;
            this.cityCounty = cityCounty;
            this.usaState = usaState;
            this.reserved = reserved;
            this.comments = comments;
        }

        private static AccountRecord from(String[] parts) throws IOException {
            if (parts.length < 3) {
                throw new IOException("Record must contain at least account number, limit, and balance");
            }
            List<String> values = new ArrayList<>(Arrays.asList(parts));
            while (values.size() < 10) {
                values.add("");
            }
            if (values.size() > 10) {
                StringBuilder mergedComments = new StringBuilder(values.get(9));
                for (int i = 10; i < values.size(); i++) {
                    if (mergedComments.length() > 0) {
                        mergedComments.append(' ');
                    }
                    mergedComments.append(values.get(i));
                }
                values = new ArrayList<>(values.subList(0, 9));
                values.add(mergedComments.toString());
            }
            BigDecimal limit = parseMoney(values.get(1));
            BigDecimal balance = parseMoney(values.get(2));
            return new AccountRecord(
                values.get(0),
                limit,
                balance,
                values.get(3),
                values.get(4),
                values.get(5),
                values.get(6),
                values.get(7),
                values.get(8),
                values.get(9)
            );
        }
    }
}
