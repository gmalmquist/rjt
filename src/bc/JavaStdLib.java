import java.util.Scanner;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

public class JavaStdLib {
  public static void main(String[] args) throws Exception {
    Scanner sc = new Scanner(System.in);
    while (true) {
      String line = sc.nextLine().trim();
      if (line.equals("END")) {
        return;
      }
      Class<?> clazz;
      try {
        clazz = Class.forName(line);
      } catch (Exception e) {
        System.out.println("NoClass");
        continue;
      }
      System.out.printf("ClassName: %s\n", clazz.getName());
      if (clazz.getSuperclass() != null) {
        System.out.printf("SuperClass: %s\n", clazz.getSuperclass().getName());
      }
      for (Class<?> superInterface : clazz.getInterfaces()) {
        System.out.printf("Implements: %s\n", superInterface.getName());
      }
      for (Method method : clazz.getDeclaredMethods()) {
        System.out.printf("MethodName: %s\n", method.getName());
        if (method.getReturnType() != null) {
          System.out.printf("MethodReturnType: %s\n", method.getReturnType().getName());
        }
        System.out.println("EndMethod");
      }
      for (Field field : clazz.getDeclaredFields()) {
        System.out.printf("FieldName: %s\n", field.getName());
        System.out.printf("FieldType: %s\n", field.getType().getName());
        System.out.println("EndField");
      }
      System.out.println("EndClass");
    }
  }
}
